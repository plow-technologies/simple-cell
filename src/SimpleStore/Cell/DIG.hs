{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-|

    This module defines the types used in the Template haskell routine in order to automate the creation of a
    higher level set of access functions to the Atomic Data in SimpleStore State.

    Most notably, it allows datatypes that look like Keyed vectors to make changes without write locking above the Key Level

    This is very important when writing to Time Series Data.

|-}

module SimpleStore.Cell.DIG (
  initializeSimpleCell
  , insertStore
  , getStore
  , updateStore
  , deleteStore
  , storeFoldrWithKey
  , storeTraverseWithKey_
  , createCellCheckPointAndClose
  ) where

-- System
import           Filesystem (removeTree,getWorkingDirectory)
import           Filesystem.Path.CurrentOS hiding (root)

-- Controls
import           Data.Bifunctor (first)
import           Control.Applicative ((<$>),(*>))
import           Control.Monad (Functor,Monad,void,when,fail,return,(>>=),fmap,(>>))
import           Data.Either (either,Either,rights,lefts)
import           Prelude                   (Ord, show, ($), (.), (==),Bool(..),putStrLn,unwords,print,(++))
import           System.IO                 (IO,hPutStrLn,stderr)


import           Control.Concurrent.STM (readTVarIO,atomically,writeTVar,STM,newTVarIO)

import           Control.Concurrent.Async (wait,async)

-- Typeclassesate

import           Data.Foldable (concat)
import           Data.Maybe (Maybe(..))
import           Data.Traversable (traverse)

import           SimpleStore.Cell.Internal (ioFoldRListT, ioFromList,
                                            ioTraverseListT_)
-- import GHC.Generics
import           Data.Serialize (Serialize)





-- ==================================================
-- STM Containers and helper functions
-- ==================================================

import qualified STMContainers.Map         as M

-- ==================================================
import qualified Data.Set                  as S

import           Plow.Extras.List (groupUp)
-- -- Strings /Monomorphs

import           Data.Text (Text)

-- Component Libraries
import           Data.Hashable (Hashable)
import           DirectedKeys.Types (DirectedKeyRaw)
import           SimpleStore (SimpleStore
                             ,getSimpleStore
                             ,putSimpleStore
                             ,makeSimpleStore
                             ,openSimpleStore
                             ,createCheckpointImmediate
                             ,createCheckpoint
                             ,StoreError(..)
                             )
import           SimpleStore.Cell.Types (CellKey(..)
                                        ,CellKeyStore(..)
                                        ,FileKey(..)
                                        ,SimpleCell(..)
                                        ,CellCore(..))



------------------------------------------------
-- END IMPORTS
------------------------------------------------







-- | All simple stores should have an empty case
-- This is the empty case for the data store for cell keys
emptyCellKeyStore :: CellKeyStore
emptyCellKeyStore = CellKeyStore S.empty



-- | Take a CellKey and convert it to a fileKey
-- A file key is what will be written as the file name
makeFileKey ::  CellKey k src dst tm st ->
               st ->
               FileKey                  
makeFileKey ck = FileKey . codeCellKeyFilename ck . getKey ck 




-- | Go from a file key (writable to a file system usually)
-- to a DirectedKeyRaw which can be used match on different pieces
unmakeFileKey :: CellKey k src dst tm st
                        -> FileKey -> Either Text (DirectedKeyRaw k src dst tm)
unmakeFileKey ck  = decodeCellKeyFilename ck . getFileKey 





-- | A parent dir (pdir) a root dir for the store (rdir) and the newStatePath
makeWorkingStatePath  :: (Functor m, Monad m) =>     FilePath -> FilePath -> Text -> m FilePath
makeWorkingStatePath pdir rdir newStatePath = do
    void $ when (newStatePath == "") (fail "--> Cell key led to empty state path")
    return $ pdir </> rdir </> fromText newStatePath






--------------------------------------------------
-- Cell Core interaction functions
-- The cell core is designed to be private... These accessors are used for other functions
-- These Functions will be made into the Simple Core
--------------------------------------------------

-- | Path manipulation happens at every atomic change to the Cell
-- They do not actually do the deletion and creation of a filepath but instead delete and create the reference to it
-- DIG FileKey interface is where the acidFunctions live They are functions of fileKey without the conversions
-- D delete
-- I insert
-- G get
deleteSimpleCellPathFileKey :: SimpleStore CellKeyStore -> FileKey -> IO ()
deleteSimpleCellPathFileKey st fk = do
  (CellKeyStore { getCellKeyStore = hsSet}) <- getSimpleStore st
  putSimpleStore st (CellKeyStore (S.delete fk hsSet ))
  void $ createTwoCheckpoints st

-- |Note... This insert is repsert functional
insertSimpleCellPathFileKey :: SimpleStore CellKeyStore -> FileKey ->  IO (SimpleStore CellKeyStore)
insertSimpleCellPathFileKey st fk =  do
  CellKeyStore { getCellKeyStore = hsSet} <- getSimpleStore st
  void $  putSimpleStore       st (CellKeyStore (S.insert  fk hsSet ))
  void $  createTwoCheckpoints st
  return st


-- | User Interface Defining Function
--   The 'st' in the type definition here is the AcidState that will be turned into a watched state
--  Warning, inserting a state that is already inserted throws an exception

insertStore :: ( Ord k  , Hashable k ,
                Ord src, Hashable src,
                Ord dst, Hashable dst,
                Ord tm , Hashable tm ,
                Serialize st) =>
                CellKey k src dst tm st
                -> SimpleCell k src dst tm st (SimpleStore CellKeyStore)
                -> st
                -> IO (SimpleStore st)
insertStore ck (SimpleCell (CellCore liveMap tvarFStore) _ pdir rdir)  st = do
  fullStatePath <- makeWorkingStatePath pdir rdir newStatePath
  fStore        <- readTVarIO tvarFStore
  _             <- insertSimpleCellPathFileKey fStore fk  
  eSimpleStore  <- makeSimpleStore fullStatePath st
  
  either (\e -> deleteSimpleCellPathFileKey fStore fk *> (fail.show) e)
         (atomicallyInsertAndWriteNewState fStore)
         eSimpleStore

 where
   fk = makeFileKey ck st
   newStatePath = codeCellKeyFilename ck.getKey ck $ st

   
   stmInsert st' = M.insert st' (getKey ck st)  liveMap

     
   atomicallyInsertAndWriteNewState fStore simpleStore = do
        atomically $ stmInsert simpleStore
        atomically $ writeTVar tvarFStore fStore
        return simpleStore








-- | Get a SimpleStore from the appropriate Cell
getStore :: (Ord k, Hashable k,
             Ord src, Hashable src,
             Ord dst, Hashable dst,
             Ord tm, Hashable tm,
             Serialize st) =>
             CellKey k src dst tm st
             -> SimpleCell k src dst tm st (SimpleStore CellKeyStore)
             -> st
             -> IO (Maybe (SimpleStore st))
getStore ck sc st = atomically (M.lookup dkr cellMap)
  where
    dkr = getKey ck st
    cellMap = ccLive.cellCore $ sc



-- | This function repserts into the store
-- should be renamed issue number #106
updateStore :: forall st k src dst tm st'.(Ord k,   Hashable k,
                                Ord src, Hashable src,
                                Ord dst, Hashable dst,
                                Ord tm,  Hashable tm) =>
                                CellKey k src dst tm st
                               -> SimpleCell k src dst tm st st' -> SimpleStore st -> st -> IO ()
updateStore ck (SimpleCell (CellCore liveMap _tvarFStore) _ _pdir _rdir )  simpleSt st =  atomically $ stmInsert simpleSt
   where
     stmInsert :: SimpleStore st -> STM ()
     stmInsert simpleSt' = do
       M.insert simpleSt' (getKey ck st) liveMap




-- | delete a given simple cell
deleteStore  :: (Ord tm, Hashable tm ,
                 Ord dst, Hashable dst,
                 Ord src, Hashable src ,
                 Ord k, Hashable k) =>
     CellKey k src dst tm st
     -> SimpleCell k src dst tm t (SimpleStore CellKeyStore)
     -> st
     -> IO ()
deleteStore ck (SimpleCell (CellCore liveMap tvarFStore) _ pdir rdir) st = do
  void (atomically stmDelete)
  fStore <- readTVarIO tvarFStore  
  void (deleteSimpleCellPathFileKey fStore fk)
  void (createTwoCheckpoints fStore)
  atomically (writeTVar tvarFStore fStore)
  np <- makeWorkingStatePath pdir rdir targetStatePath
  removeTree np
     where
        fk              = makeFileKey ck st
        targetStatePath = (codeCellKeyFilename ck.getKey ck) st
        stmDelete       = M.delete (getKey ck st)
                                    liveMap

storeFoldrWithKey :: t6
     -> SimpleCell t t1 t2 t3 t5 t4
     -> (t6 -> DirectedKeyRaw t t1 t2 t3 -> t5 -> IO b -> IO b)
     -> IO b
     -> IO b
storeFoldrWithKey ck (SimpleCell (CellCore tlive _) _ _ _) fldFcn seed = do
  let
    keyValueListT = M.stream $ tlive

  innerIO <- ioFoldRListT (\ (key,simpleSt) b -> do
                                        st <- getSimpleStore simpleSt
                                        fldFcn ck key st b)
                           seed keyValueListT
  innerIO

storeTraverseWithKey_ :: t5 -> SimpleCell t t1 t2 t3 t6 t4
     -> (t5 -> DirectedKeyRaw t t1 t2 t3 -> t6 -> IO ())
     -> IO ()
storeTraverseWithKey_ ck (SimpleCell (CellCore tlive _) _ _ _) tvFcn  = do
  ioTraverseListT_ (\(key, cs) -> do
                     st <- getSimpleStore cs
                     tvFcnWrp key st) listTMapWrapper
          where
            tvFcnWrp        = tvFcn    ck
            listTMapWrapper = M.stream tlive

            


-- SimpleCell k src dst tm st st'

createCellCheckPointAndClose :: forall k src dst tm st .  SimpleCell k src dst tm st (SimpleStore CellKeyStore) -> IO ()
createCellCheckPointAndClose (SimpleCell (CellCore _ tvarFStore) _ _pdir _rdir ) = do  
  fStore <- readTVarIO tvarFStore
  void $ createCheckpointImmediate fStore 








initializeSimpleCell :: (Data.Serialize.Serialize stlive ,
                         Ord tm, Hashable tm ,
                         Ord dst, Hashable dst ,
                         Ord src, Hashable src ,
                         Ord k, Hashable k ) =>
     CellKey k src dst tm stlive
     -> stlive
     -> Text
     -> IO
          (SimpleCell
             k
             src
             dst
             tm
             stlive
             (SimpleStore CellKeyStore))
initializeSimpleCell ck emptyTargetState root  = do
 parentWorkingDir <- getWorkingDirectory
 let fpr            = parentWorkingDir </> simpleRootPath
 putStrLn "opening simple-cell"
 simpleStoreState <- openSimpleStore fpr  >>= either (\e -> do 
                                                                      hPutStrLn stderr (show e)
                                                                      if shouldInitializeFail e
                                                                      then do 
                                                                          hPutStrLn stderr (show e)
                                                                          eCellKeyStore <- makeSimpleStore fpr emptyCellKeyStore
                                                                          either (\_ -> fail "cellKey won't initialize" ) return  eCellKeyStore
                                                                      else
                                                                          fail "data appears corrupted"
                                                                  ) return  ::  IO (SimpleStore CellKeyStore)
 putStrLn "getting Key"                                                  
 fkSet   <-  getCellKeyStore <$>  getSimpleStore simpleStoreState :: IO (S.Set FileKey)

 let setEitherFileKeyRaw = S.map (unmakeFileKey ck) fkSet
 let groupedList = groupUp 16 (rights . S.toList $ setEitherFileKeyRaw) 
 putStrLn "traverse and wait"
 aStateList <- traverse (traverseAndWait fpr) groupedList 
 let
  eitherStateList = Data.Foldable.concat aStateList 
  stateList  =  rights  eitherStateList
  errorList  =  unwords . lefts . fmap (first show) $ eitherStateList
 hPutStrLn stderr errorList
 putStrLn "state list"
 stateMap <-  ioFromList stateList
 putStrLn "fAcidState"
 tvarFAcid <- newTVarIO simpleStoreState
 putStrLn "return"
 return $ SimpleCell (CellCore stateMap tvarFAcid) ck parentWorkingDir newWorkingDir
  where
      newWorkingDir  = simpleRootPath
      simpleRootPath = fromText root
      traverseAndWait fp l = do
        aRes <- traverse (traverseLFcn fp) l
        traverse wait aRes
      traverseLFcn  fp fkRaw = async $ traverseLFcn' fp fkRaw
      traverseLFcn' fp fkRaw = do
        let fpKey = fp </> (fromText . codeCellKeyFilename ck $ fkRaw)
        est' <- openCKSt fpKey emptyTargetState
        print $ "opened: " ++ show fpKey
        return $ fmap (\st' -> (fkRaw, st')) est'





openCKSt :: Serialize st =>
             FilePath -> st -> IO (Either StoreError (SimpleStore st))
openCKSt fpKey _emptyTargetState = openSimpleStore fpKey

-- -- | Exception and Error handling
-- should the initialize wipe the state or fail.
shouldInitializeFail :: StoreError -> Bool
shouldInitializeFail  StoreFolderNotFound  = True
shouldInitializeFail  _                     = False


createTwoCheckpoints  :: Serialize st => SimpleStore st -> IO (Either StoreError ())
createTwoCheckpoints fStore = (createCheckpoint fStore) >> (createCheckpointImmediate fStore)
