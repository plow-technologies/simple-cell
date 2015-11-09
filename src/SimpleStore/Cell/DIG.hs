{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
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





-- -- System
import           Filesystem
import           Filesystem.Path.CurrentOS hiding (root)

-- -- Controls
import           Control.Applicative
import           Control.Monad
import           Data.Either
import           Prelude                   (Ord, show, ($), (.), (==))
import           System.IO                 (IO)

-- import CorePrelude hiding (try,catch, finally)
import           Control.Concurrent.STM
-- import Control.Monad.Reader ( ask )
-- import Control.Monad.State

import           Control.Concurrent.Async


-- Typeclassesate

import           Data.Foldable
import           Data.Maybe
import           Data.Traversable
import           SimpleStore.Cell.Internal (ioFoldRListT, ioFromList,
                                            ioTraverseListT_)
-- import GHC.Generics
import           Data.Serialize


-- -- Component Libraries
-- import DirectedKeys.Types

-- -- Containers
-- import qualified Data.Map.Strict as M

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



emptyCellKeyStore :: CellKeyStore
emptyCellKeyStore = CellKeyStore S.empty


makeFileKey :: CellKey k src dst tm st -> st -> FileKey
makeFileKey ck s = FileKey (codeCellKeyFilename ck . getKey ck $ s)

unmakeFileKey :: CellKey k src dst tm st
                        -> FileKey -> Either Text (DirectedKeyRaw k src dst tm)
unmakeFileKey ck s = decodeCellKeyFilename ck . getFileKey $ s

makeWorkingStatePath  :: (Functor m, Monad m) =>     FilePath -> FilePath -> Text -> m FilePath
makeWorkingStatePath pdir rdir nsp = do
    void $ when (nsp == "") (fail "--> Cell key led to empty state path")
    return $ pdir </> rdir </> fromText nsp

-- |Cell Core interaction functions
-- The cell core is designed to be private... These accessors are used for other functions
-- These Functions will be made into the Simple Core


-- | Path manipulation happens at every atomic change to the Cell
-- These functions are made acidic
-- They do not actually do the deletion and creation of a filepath but instead delete and create the reference to it

-- | DIG FileKey interface is where the acidFunctions live They are functions of fileKey without the conversions

deleteSimpleCellPathFileKey :: SimpleStore CellKeyStore -> FileKey -> IO ()
deleteSimpleCellPathFileKey st fk = do
  (CellKeyStore { getCellKeyStore = hsSet}) <- getSimpleStore st
  putSimpleStore st (CellKeyStore (S.delete fk hsSet ))
  void $ createCheckpoint st


-- |Note... This insert is repsert functional
insertSimpleCellPathFileKey :: SimpleStore CellKeyStore -> FileKey ->  IO (SimpleStore CellKeyStore)
insertSimpleCellPathFileKey st fk =  do
  (CellKeyStore { getCellKeyStore = hsSet}) <- getSimpleStore st
  putSimpleStore st  $ CellKeyStore (S.insert  fk hsSet )
  void $ createCheckpoint st
  return st


-- getSimpleCellPathFileKey :: SimpleStore CellKeyStore -> IO CellKeyStore
-- getSimpleCellPathFileKey = getSimpleStore



-- | User Interface Defining Function

-- | The 'st' in the type definition here is the AcidState that will be turned into a watched state

-- | Warning, inserting a state that is already inserted throws an exception

insertStore :: (Ord k  , Hashable k ,
                Ord src, Hashable src,
                Ord dst, Hashable dst,
                Ord tm , Hashable tm ,
                Serialize st) =>
                CellKey k src dst tm st
                -> SimpleCell k src dst tm st (SimpleStore CellKeyStore)
                -> st
                -> IO (SimpleStore st)
insertStore ck (SimpleCell (CellCore liveMap tvarFStore) _ pdir rdir)  st = do
  let newStatePath = codeCellKeyFilename ck.getKey ck $ st
  fullStatePath <- makeWorkingStatePath pdir rdir newStatePath
  let fk = makeFileKey ck st
  fStore <- readTVarIO tvarFStore
  void $ insertSimpleCellPathFileKey fStore fk
  eSimpleStore <- makeSimpleStore fullStatePath st
  case eSimpleStore of
    Left e -> deleteSimpleCellPathFileKey fStore fk >> (fail.show $ e)
    Right simpleStore -> do
      atomically $ stmInsert simpleStore
      atomically $ writeTVar tvarFStore fStore
      return simpleStore
     where
       stmInsert st' = do
--         liveMap <- readTVar tlive
         M.insert st' (getKey ck st)  liveMap

getStore :: (Ord k, Hashable k,
             Ord src, Hashable src,
             Ord dst, Hashable dst,
             Ord tm, Hashable tm,
             Serialize st) =>
             CellKey k src dst tm st
             -> SimpleCell k src dst tm st (SimpleStore CellKeyStore)
             -> st
             -> IO (Maybe (SimpleStore st))
getStore ck sc st = atomically $ (M.lookup dkr) ( cellMap )
  where
    dkr = getKey ck st
    cellMap = ccLive.cellCore $ sc



updateStore
  :: (Ord k,   Hashable k,
      Ord src, Hashable src,
      Ord dst, Hashable dst,
      Ord tm,  Hashable tm) =>
     CellKey k src dst tm st
     -> SimpleCell k src dst tm st st' -> SimpleStore st -> st -> IO ()
updateStore ck (SimpleCell (CellCore liveMap _tvarFStore) _ _pdir _rdir )  simpleSt st =  atomically $ stmInsert simpleSt
   where
     stmInsert :: SimpleStore st -> STM ()
     stmInsert simpleSt' = do
       M.insert simpleSt' (getKey ck st)  liveMap


deleteStore  :: (Ord tm, Hashable tm ,
                 Ord dst, Hashable dst,
                 Ord src, Hashable src ,
                 Ord k, Hashable k) =>
     CellKey k src dst tm st
     -> SimpleCell k src dst tm t (SimpleStore CellKeyStore)
     -> st
     -> IO ()

deleteStore ck (SimpleCell (CellCore liveMap tvarFStore) _ pdir rdir) st = do
  let targetStatePath = codeCellKeyFilename ck.getKey ck $ st
  void $ atomically stmDelete
  fStore <- readTVarIO tvarFStore
  let fk = makeFileKey ck st
  void $ deleteSimpleCellPathFileKey fStore fk
  void $ createCheckpoint fStore
  atomically $ writeTVar tvarFStore fStore
  np <- makeWorkingStatePath pdir rdir targetStatePath
  removeTree np
     where
        stmDelete = do
          M.delete (getKey ck st) liveMap


storeFoldrWithKey
  :: t6
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
  let listTMapWrapper = M.stream tlive
  ioTraverseListT_ (\(key, cs) -> do
                     st <- getSimpleStore cs
                     tvFcnWrp key st) listTMapWrapper
          where
            tvFcnWrp =  tvFcn ck



createCellCheckPointAndClose ::   SimpleCell t t1 t2 t3 st (SimpleStore CellKeyStore) -> IO ()
createCellCheckPointAndClose (SimpleCell (CellCore liveMap tvarFStore) _ _pdir _rdir ) = do
  let listTMapWrapper = M.stream liveMap
  void $ ioTraverseListT_ (\(_,v) -> closeSimpleStore v )  listTMapWrapper
  fStore <- readTVarIO tvarFStore
  void $ createCheckpoint fStore >> closeSimpleStore fStore


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
initializeSimpleCell ck emptyTargetState root = do
 parentWorkingDir <- getWorkingDirectory
 let simpleRootPath = fromText root
     newWorkingDir = simpleRootPath
     fpr           = parentWorkingDir </> simpleRootPath

 fAcidSt <- openSimpleStore  fpr  >>= either (\_ -> do
                                                      eCellKeyStore <- makeSimpleStore fpr emptyCellKeyStore
                                                      either (\_ -> fail "cellKey won't initialize" ) return  eCellKeyStore
                                                  ) return  ::  IO (SimpleStore CellKeyStore)
 fkSet   <-  getCellKeyStore <$>  getSimpleStore fAcidSt :: IO (S.Set FileKey)

 let setEitherFileKeyRaw = S.map (unmakeFileKey ck) fkSet
 let groupedList = groupUp 16 (rights . S.toList $ setEitherFileKeyRaw)
 aStateList <- traverse (traverseAndWait fpr) groupedList
 let stateList =  rights.rights $ Data.Foldable.concat aStateList
 stateMap <-  ioFromList stateList
 tvarFAcid <- newTVarIO fAcidSt
 return $ SimpleCell (CellCore stateMap tvarFAcid) ck parentWorkingDir newWorkingDir
  where
      traverseAndWait fp l = do
        aRes <- traverse (traverseLFcn fp) l
        traverse waitCatch aRes
      traverseLFcn  fp fkRaw = async $ traverseLFcn' fp fkRaw
      traverseLFcn' fp fkRaw = do
        let fpKey = fp </> (fromText . codeCellKeyFilename ck $ fkRaw)
        est' <- openCKSt fpKey emptyTargetState
--        print $ "opened: " ++ show fpKey
        return $ fmap (\st' -> (fkRaw, st')) est'



openCKSt :: Serialize st =>
             FilePath -> st -> IO (Either StoreError (SimpleStore st))
openCKSt fpKey _emptyTargetState = openSimpleStore fpKey

-- -- | Exception and Error handling
-- -- type AEither a = Either StoreCellErrora


