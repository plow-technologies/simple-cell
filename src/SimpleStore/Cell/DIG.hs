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
  , initializeSimpleCellAndErrors
  , insertStore
  , getStore
  , repsertStore
  , deleteStore
  , storeFoldrWithKey
  , storeTraverseWithKey_
  , createCellCheckPointAndClose
  ) where

-- System
import           Filesystem
import           Filesystem.Path.CurrentOS hiding (root)

-- Controls
import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Either
import           Prelude                   (Ord, show, ($), (.), (==),Bool(..),putStrLn,otherwise,null,concat,snd)
import           System.IO                 (IO,hPutStrLn,stderr,hPrint)

-- Typeclassesate

import           Data.Foldable
import           Data.Maybe
import           Data.Traversable
import           SimpleStore.Cell.Internal (ioFoldRListT, ioFromList,
                                            ioTraverseListT_)
import           Data.Serialize

-- ==================================================
-- STM Containers and helper functions
-- ==================================================

import qualified STMContainers.Map         as M

-- ==================================================
import qualified Data.Set                  as S

import           Plow.Extras.List

-- -- Strings /Monomorphs
import           Data.Text

-- Component Libraries
import           Data.Hashable
import           DirectedKeys.Types
import           SimpleStore
import           SimpleStore.Cell.Types

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
makeWorkingStatePath  :: (Monad m) => FilePath -> FilePath -> Text -> m FilePath
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
             Ord tm, Hashable tm) =>
                SimpleCell k src dst tm st (SimpleStore CellKeyStore)
             -> DirectedKeyRaw k src dst tm
             -> IO (Maybe (SimpleStore st))
getStore sc dkr = atomically (M.lookup dkr cellMap)
  where
    cellMap = ccLive.cellCore $ sc

-- | This function repserts into the store
-- should be renamed issue number #106
repsertStore :: forall st k src dst tm st'.(Ord k,   Hashable k,
                                Ord src, Hashable src,
                                Ord dst, Hashable dst,
                                Ord tm,  Hashable tm) =>
                                CellKey k src dst tm st
                               -> SimpleCell k src dst tm st st' -> SimpleStore st -> st -> IO ()
repsertStore ck (SimpleCell (CellCore liveMap _tvarFStore) _ _pdir _rdir )  simpleSt st =  atomically $ stmInsert simpleSt
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
     -> DirectedKeyRaw k src dst tm
     -> IO ()
deleteStore ck (SimpleCell (CellCore liveMap tvarFStore) _ pdir rdir) dkr = do
  void (atomically stmDelete)
  fStore <- readTVarIO tvarFStore
  void (deleteSimpleCellPathFileKey fStore fk)
  void (createTwoCheckpoints fStore)
  atomically (writeTVar tvarFStore fStore)
  np <- makeWorkingStatePath pdir rdir targetStatePath
  removeTree np
     where
        fk              = FileKey targetStatePath
        targetStatePath = codeCellKeyFilename ck dkr
        stmDelete       = M.delete dkr
                                    liveMap

storeFoldrWithKey :: ck
     -> SimpleCell k src dst tm stlive stlive           
     -> (ck -> DirectedKeyRaw k src dst tm -> stlive -> IO b -> IO b)
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

storeTraverseWithKey_ :: ck -> SimpleCell k src dst tm stlive stlive
     -> (ck -> DirectedKeyRaw k src dst tm -> stlive -> IO ())
     -> IO ()
storeTraverseWithKey_ ck (SimpleCell (CellCore tlive _) _ _ _) tvFcn  = do
  ioTraverseListT_ (\(key, cs) -> do
                     st <- getSimpleStore cs
                     tvFcnWrp key st) listTMapWrapper
          where
            tvFcnWrp        = tvFcn    ck
            listTMapWrapper = M.stream tlive






createCellCheckPointAndClose :: (SimpleCell k src dst tm st (SimpleStore CellKeyStore))   -> IO ()
createCellCheckPointAndClose    (SimpleCell (CellCore _ tvarFStore) _ _pdir _rdir ) =  do
  fStore <- readTVarIO tvarFStore
  void (createCheckpoint fStore)




initializeSimpleCell'
  :: forall k tm dst src stlive. (Data.Serialize.Serialize stlive , Ord tm, Hashable tm , Ord dst, Hashable dst , Ord src, Hashable src , Ord k, Hashable k )
  => CellKey k src dst tm stlive
  -> stlive
  -> Text
  -> IO ([StoreError], SimpleCell k src dst tm stlive (SimpleStore CellKeyStore))
initializeSimpleCell' ck emptyTargetState root  = do
 parentWorkingDir   <- getWorkingDirectory
 let simpleRootPath = fromText root
     newWorkingDir  = simpleRootPath
     fpr            = parentWorkingDir </> simpleRootPath
 putStrLn "opening simple-cell"
 fAcidSt <- openSimpleStore fpr  >>= either (\e -> do
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
 fkSet   <-  getCellKeyStore <$>  getSimpleStore fAcidSt :: IO (S.Set FileKey)

 let setEitherFileKeyRaw = S.map (unmakeFileKey ck) fkSet                :: S.Set (Either Text (DirectedKeyRaw k src dst tm))
 let groupedList = groupUp 16 (rights . S.toList $ setEitherFileKeyRaw)

 putStrLn "traverse and wait"
 aStateList <- traverse (traverseAndWait fpr) groupedList :: IO [[Either  StoreError (DirectedKeyRaw k src dst tm, SimpleStore stlive)]]
 let stateList = rights $ Data.Foldable.concat aStateList
     errorList = lefts $ Data.Foldable.concat aStateList
 putStrLn "state list"
 stateMap <-  ioFromList stateList
 putStrLn "fAcidState"
 tvarFAcid <- newTVarIO fAcidSt
 putStrLn "return"
 _       <- logAllLefts (S.toList $ setEitherFileKeyRaw) (Prelude.concat aStateList)
 return $ (errorList, (SimpleCell (CellCore stateMap tvarFAcid) ck parentWorkingDir newWorkingDir))
  where
      traverseAndWait fp l = do
        aRes <- traverse (traverseLFcn fp) l
        traverse wait aRes
      traverseLFcn  fp fkRaw = async $ traverseLFcn' fp fkRaw
      traverseLFcn' fp fkRaw = do
        let fpKey = fp </> (fromText . codeCellKeyFilename ck $ fkRaw)
        est' <- openCKSt fpKey emptyTargetState
--        print $ "opened: " ++ show fpKey
        return $ fmap (\st' -> (fkRaw, st')) est'







-- | Initialize SimpleCell
initializeSimpleCell
  :: forall k tm dst src stlive. (Data.Serialize.Serialize stlive , Ord tm, Hashable tm , Ord dst, Hashable dst , Ord src, Hashable src , Ord k, Hashable k)
  => CellKey k src dst tm stlive
  -> stlive
  -> Text
  -> IO (SimpleCell k src dst tm stlive (SimpleStore CellKeyStore))
initializeSimpleCell ck emptyTargetState root = snd <$> initializeSimpleCell' ck emptyTargetState root


-- | Initialize SimpleCell and return errors together with SimpleCell.
-- The errors and cell are wrapped in 'InitializedCell' record type.
initializeSimpleCellAndErrors
  :: forall k tm dst src stlive. (Data.Serialize.Serialize stlive , Ord tm, Hashable tm , Ord dst, Hashable dst , Ord src, Hashable src , Ord k, Hashable k)
  => CellKey k src dst tm stlive
  -> stlive
  -> Text
  -> IO (InitializedCell k src dst tm stlive (SimpleStore CellKeyStore))
initializeSimpleCellAndErrors ck emptyTargetState root = do
  (errors, cell) <- initializeSimpleCell' ck emptyTargetState root
  pure $ InitializedCell cell errors



logAllLefts  :: [Either Text a] -> [Either StoreError b] -> IO ()
logAllLefts directedKeyErrors stateList = logDirectedKeyErrors  *> logStoreErrors
  where
    logDirectedKeyErrors
         | Prelude.null directedKeyErrors = return ()
         | otherwise              = hPutStrLn stderr "Directed Key errors" *>
                                    hPrint    stderr  (lefts directedKeyErrors)
    logStoreErrors
         | Prelude.null stateList = return ()
         | otherwise              = hPutStrLn stderr "StoreErrors " *>
                                    hPrint    stderr  (lefts stateList)

openCKSt :: Serialize st =>
             FilePath -> st -> IO (Either StoreError (SimpleStore st))
openCKSt fpKey _emptyTargetState = openSimpleStore fpKey

-- -- | Exception and Error handling
-- should the initialize wipe the state or fail.
shouldInitializeFail :: StoreError -> Bool
shouldInitializeFail  StoreFolderNotFound = True
shouldInitializeFail  _                 = False


createTwoCheckpoints  :: Serialize st => SimpleStore st -> IO (Either StoreError ())
createTwoCheckpoints fStore = (createCheckpoint fStore) >> (createCheckpointImmediate fStore)
