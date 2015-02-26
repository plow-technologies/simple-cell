{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies,ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-} -- For CellStateConstraint (from SimpleStore.Cell.Types)
{-| 

    This module defines the types used in the Template haskell routine in order to automate the creation of a
    higher level set of access functions to the Atomic Data in SimpleStore State.

    Most notably, it allows datatypes that look like Keyed vectors to make changes without write locking above the Key Level

    This is very important when writing to Time Series Data.  
    
|-} 



module SimpleStore.Cell.DIG (
  initializeSimpleCell
  , createCellCheckPointAndClose
  ) where





-- -- System 
import           Filesystem.Path.CurrentOS hiding (root)
import           Filesystem

-- -- Controls
import           Prelude (show ,($), (.),Ord, (==))
import           Data.Either
import           System.IO (IO)
import           Control.Monad
import           Control.Applicative

-- import CorePrelude hiding (try,catch, finally)
import           Control.Concurrent.STM
-- import Control.Monad.Reader ( ask )
-- import Control.Monad.State  

import           Control.Concurrent.Async


-- Typeclassesate

import           Data.Foldable
import           Data.Maybe
import           Data.Traversable
import           SimpleStore.Cell.Internal (ioFoldRListT
                                           , ioTraverseListT_
                                           , ioFromList )
import           SimpleStore.Cell.Types hiding (Cell(..))
import qualified SimpleStore.Cell.Types as CT (Cell(..))
-- import GHC.Generics
import           Data.Serialize


-- -- Component Libraries
-- import DirectedKeys.Types

-- -- Containers 
-- import qualified Data.Map.Strict as M

-- ==================================================
-- STM Containers and helper functions
-- ==================================================

import qualified STMContainers.Map as M


-- ==================================================
import qualified Data.Set as S

import           Plow.Extras.List
-- -- Strings /Monomorphs 

import           Data.Text

-- Component Libraries
import           DirectedKeys.Types
import Data.Hashable
import           SimpleStore



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

insertStore :: (CellStateConstraint k src dst tm st) =>
                SimpleCell k src dst tm st (SimpleStore CellKeyStore)
                -> st
                -> IO (SimpleStore st)                     
insertStore (SimpleCell (CellCore liveMap tvarFStore) _ pdir rdir)  st = do
  let ck = simpleCellKey
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

getStore :: (CellStateConstraint k src dst tm st) =>
             SimpleCell k src dst tm st (SimpleStore CellKeyStore)
             -> st
             -> IO (Maybe (SimpleStore st))
getStore sc st = atomically $ (M.lookup dkr) ( cellMap )
  where
    ck = simpleCellKey
    dkr = getKey ck st
    cellMap = ccLive.cellCore $ sc



updateStore
  :: (CellStateConstraint k src dst tm st) =>
     SimpleCell k src dst tm st st' -> SimpleStore st -> st -> IO ()
updateStore (SimpleCell (CellCore liveMap _tvarFStore) _ _pdir _rdir )  simpleSt st =  atomically $ stmInsert simpleSt
   where 
     ck = simpleCellKey
     stmInsert st' = do 
       M.insert st' (getKey ck st)  liveMap


deleteStore :: (CellStateConstraint k src dst tm st) =>
     SimpleCell k src dst tm st (SimpleStore CellKeyStore)
     -> st
     -> IO ()

deleteStore (SimpleCell (CellCore liveMap tvarFStore) _ pdir rdir) st = do 
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
        ck = simpleCellKey
        stmDelete = do 
          M.delete (getKey ck st) liveMap


storeFoldrWithKey
  :: (CellStateConstraint k src dst tm stlive) =>
     SimpleCell k src dst tm stlive stdormant
     -> (CellKey k src dst tm stlive -> DirectedKeyRaw k src dst tm -> stlive -> IO b -> IO b)
     -> IO b
     -> IO b
storeFoldrWithKey (SimpleCell (CellCore tlive _) _ _ _) fldFcn seed = do 
  let
    keyValueListT = M.stream $ tlive
    ck = simpleCellKey

  innerIO <- ioFoldRListT (\ (key,simpleSt) b -> do
                                        st <- getSimpleStore simpleSt
                                        fldFcn ck key st b)
                           seed keyValueListT
  innerIO                   



storeTraverseWithKey_ :: (CellStateConstraint k src dst tm stlive) =>
     SimpleCell k src dst tm stlive stdormant
     -> (CellKey k src dst tm stlive -> DirectedKeyRaw k src dst tm -> stlive -> IO ())
     -> IO ()

storeTraverseWithKey_ (SimpleCell (CellCore tlive _) _ _ _) tvFcn  = do 
  let listTMapWrapper = M.stream tlive
  ioTraverseListT_ (\(key, cs) -> do
                     st <- getSimpleStore cs
                     tvFcnWrp key st) listTMapWrapper
          where
            ck = simpleCellKey
            tvFcnWrp =  tvFcn ck



createCellCheckPointAndClose ::   SimpleCell k src dst tm st (SimpleStore CellKeyStore) -> IO ()
createCellCheckPointAndClose (SimpleCell (CellCore liveMap tvarFStore) _ _pdir _rdir ) = do 
  let listTMapWrapper = M.stream liveMap
  void $ ioTraverseListT_ (\(_,v) -> closeSimpleStore v )  listTMapWrapper
  fStore <- readTVarIO tvarFStore
  void $ createCheckpoint fStore >> closeSimpleStore fStore


initializeSimpleCell :: (Ord tm, Hashable tm ,
                         Ord dst, Hashable dst ,
                         Ord src, Hashable src ,
                         Ord k, Hashable k,
                         SimpleCellState stlive,
                         k ~ SimpleCellKey stlive,
                         src ~ SimpleCellSrc stlive,
                         dst ~ SimpleCellDst stlive,
                         tm ~ SimpleCellDateTime stlive,
                         Serialize stlive) =>
     stlive
     -> Text
     -> IO
          (SimpleCell
             k
             src
             dst
             tm
             stlive
             (SimpleStore CellKeyStore))
initializeSimpleCell emptyTargetState root = do 
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
      ck = simpleCellKey
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

instance (stdormant ~ SimpleStore CellKeyStore, CellStateConstraint k src dst tm stlive) => CT.Cell (SimpleCell k src dst tm stlive stdormant) where
  type CellLiveStateType (SimpleCell k src dst tm stlive stdormant)    = stlive
  type CellDormantStateType (SimpleCell k src dst tm stlive stdormant) = stdormant
  insertStore           = insertStore
  getStore              = getStore
  updateStore           = updateStore
  deleteStore           = deleteStore
  foldrStoreWithKey     = storeFoldrWithKey
  traverseStoreWithKey_ = storeTraverseWithKey_
   


   
  
-- -- | Exception and Error handling
-- -- type AEither a = Either StoreCellErrora


