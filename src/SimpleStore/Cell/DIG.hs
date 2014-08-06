{-# LANGUAGE OverloadedStrings, NoImplicitPrelude,TemplateHaskell, GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# LANGUAGE RecordWildCards, TypeFamilies,DeriveDataTypeable,ScopedTypeVariables #-}

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
  , storeFoldlWithKey
  , storeTraverseWithKey
  , createCellCheckPointAndClose
  ) where





-- -- System 
import           Filesystem.Path.CurrentOS hiding (root)
import           Filesystem

-- -- Controls
import           Prelude (show, (++) ,($), (.),Ord, (==) , print)
import           Data.Either
import           System.IO (IO)
import           Control.Monad
import           Control.Applicative
import           Control.Exception
-- import CorePrelude hiding (try,catch, finally)
import           Control.Concurrent.STM
-- import Control.Monad.Reader ( ask )
-- import Control.Monad.State  

import           Control.Concurrent.Async


-- Typeclassesate

import           Data.Foldable
import           Data.Maybe
import           Data.Traversable
-- import GHC.Generics
import           Data.Serialize


-- -- Component Libraries
-- import DirectedKeys.Types

-- -- Containers 
import qualified Data.Map as M
import qualified Data.Set as S

import           Plow.Extras.List
-- -- Strings /Monomorphs 

import           Data.Text

-- Component Libraries
import           DirectedKeys.Types
import           SimpleStore.Cell.Types
import           SimpleStore



emptyCellKeyStore :: CellKeyStore
emptyCellKeyStore = CellKeyStore S.empty

  
makeFileKey :: CellKey k src dst tm st -> st -> FileKey 
makeFileKey ck s = FileKey (codeCellKeyFilename ck . getKey ck $ s)

unmakeFileKey :: CellKey k src dst tm st
                        -> FileKey -> Either Text (DirectedKeyRaw k src dst tm)
unmakeFileKey ck s = (decodeCellKeyFilename ck).getFileKey $ s

makeWorkingStatePath pdir rdir nsp = do 
    void $ when (nsp == "") (fail "--> Cell key led to empty state path")
    return $ pdir </> rdir </> (fromText nsp)

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
  


-- |Note... This insert is repsert functional
insertSimpleCellPathFileKey :: SimpleStore CellKeyStore -> FileKey ->  IO (SimpleStore CellKeyStore)
insertSimpleCellPathFileKey st fk =  do 
  (CellKeyStore { getCellKeyStore = hsSet}) <- getSimpleStore st
  putSimpleStore st  $ (CellKeyStore (S.insert  fk hsSet ))
  return st


getSimpleCellPathFileKey :: SimpleStore CellKeyStore -> IO (CellKeyStore )
getSimpleCellPathFileKey st = getSimpleStore st
  
 

-- | User Interface Defining Function

-- | The 'st' in the type definition here is the AcidState that will be turned into a watched state

-- | Warning, inserting a state that is already inserted throws an exception 

insertStore :: (Ord k, Ord src, Ord dst, Ord tm,Serialize st) =>
                     CellKey k src dst tm st
                     -> SimpleCell k src dst tm st (SimpleStore CellKeyStore)
                     -> st
                     -> IO (SimpleStore st)                     
insertStore ck (SimpleCell (CellCore tlive tvarFStore) _ pdir rdir)  st = do
  let newStatePath = (codeCellKeyFilename ck).(getKey ck) $ st
  fullStatePath <- makeWorkingStatePath pdir rdir newStatePath
  let fk = makeFileKey ck st
  fStore <- readTVarIO tvarFStore
  void $ insertSimpleCellPathFileKey fStore fk    
  eSimpleStore <- (makeSimpleStore fullStatePath st )
  case eSimpleStore of
    Left e -> (deleteSimpleCellPathFileKey fStore fk) >> (fail.show $ e)
    Right simpleStore -> do 
      atomically (stmInsert simpleStore)                                
      atomically $ writeTVar tvarFStore fStore
      return simpleStore
     where 
       stmInsert st' = do 
         liveMap <- readTVar tlive        
         writeTVar tlive $ M.insert (getKey ck st) st' liveMap

getStore :: (Ord k, Ord src, Ord dst, Ord tm,Serialize st) =>
                     CellKey k src dst tm st
                     -> SimpleCell k src dst tm st (SimpleStore CellKeyStore)
                     -> st
                     -> IO (Maybe (SimpleStore st))
getStore ck sc st = readTVarIO cMap >>= return.(M.lookup dkr )
  where
    dkr = getKey ck st
    cMap = ccLive.cellCore $ sc



updateStore
  :: (Ord t3, Ord t2, Ord t1, Ord t) =>
     CellKey t t1 t2 t3 st
     -> SimpleCell t t1 t2 t3 t4 t5 -> SimpleStore t4 -> st -> IO ()
updateStore ck (SimpleCell (CellCore tlive _tvarFStore) _ _pdir _rdir )  simpleSt st = do
  atomically $ stmInsert simpleSt
   where 
     stmInsert st' = do 
       liveMap <- readTVar tlive        
       writeTVar tlive $ M.insert (getKey ck st) st' liveMap


deleteStore  :: (Ord tm, Ord dst, Ord src, Ord k) =>
     CellKey k src dst tm st
     -> SimpleCell k src dst tm t (SimpleStore CellKeyStore)
     -> st
     -> IO ()
deleteStore ck (SimpleCell (CellCore tlive tvarFStore) _ pdir rdir) st = do 
  let targetStatePath = (codeCellKeyFilename ck).(getKey ck) $ st 
  void $ atomically stmDelete
  fStore <- readTVarIO tvarFStore
  let fk = makeFileKey ck st  
  void $ deleteSimpleCellPathFileKey fStore fk
  void $ createCheckpoint fStore
  atomically $ writeTVar tvarFStore fStore
  np <- (makeWorkingStatePath pdir rdir targetStatePath)
  removeTree np
     where
        stmDelete = do 
          liveMap <- readTVar tlive
          writeTVar tlive $ M.delete (getKey ck st) liveMap

storeFoldlWithKey :: t6  -> SimpleCell t t1 t2 t3 t5 t4 -> (t6 -> DirectedKeyRaw t t1 t2 t3 -> t5 -> IO b -> IO b)
                     -> IO b
                     -> IO b
storeFoldlWithKey ck (SimpleCell (CellCore tlive _) _ _ _) fldFcn seed = do 
  liveMap <- readTVarIO tlive 
  M.foldWithKey (\key simpleSt b -> do
                             st <- getSimpleStore simpleSt
                             fldFcn ck key st  b) seed liveMap


storeTraverseWithKey :: t5 -> SimpleCell t t1 t2 t3 t6 t4
     -> (t5 -> DirectedKeyRaw t t1 t2 t3 -> t6 -> IO b)
     -> IO (M.Map (DirectedKeyRaw t t1 t2 t3) b)
storeTraverseWithKey ck (SimpleCell (CellCore tlive _) _ _ _) tvFcn  = do 
  liveMap <- readTVarIO tlive 
  M.traverseWithKey (\key cs -> do
                        st <- getSimpleStore cs
                        tvFcnWrp key st)  liveMap
      where
        tvFcnWrp k a = do
          ( tvFcn ck k a)
          

createCellCheckPointAndClose ::   SimpleCell t t1 t2 t3 st (SimpleStore CellKeyStore) -> IO ()
createCellCheckPointAndClose (SimpleCell (CellCore tlive tvarFStore) _ _pdir _rdir ) = do 
  liveMap <- readTVarIO tlive 
  void $ traverse (\st -> (closeSimpleStore st)   ) liveMap
  fStore <- readTVarIO tvarFStore
  void $ createCheckpoint fStore >> closeSimpleStore fStore


initializeSimpleCell :: (Data.Serialize.Serialize stlive, Ord tm, Ord dst, Ord src, Ord k) =>
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
     fpr           = (parentWorkingDir </> simpleRootPath)

 fAcidSt <- (openSimpleStore ( fpr )) >>= (either (\_ -> do
                                                      eCellKeyStore <- (makeSimpleStore fpr emptyCellKeyStore )
                                                      either (\_ -> fail "cellKey won't initialize" ) (return ) eCellKeyStore
                                                  ) (return) ) ::  IO (SimpleStore CellKeyStore)
 fkSet   <-  getCellKeyStore <$>  getSimpleStore fAcidSt :: IO (S.Set FileKey)

 let setEitherFileKeyRaw = S.map (unmakeFileKey ck) fkSet  
 let groupedList = groupUp 16 (rights . S.toList $ setEitherFileKeyRaw)  
 aStateList <- (traverse (traverseAndWait fpr) groupedList) 
 let stateList =  rights.rights $ (Data.Foldable.concat aStateList)
 let stateMap = M.fromList stateList

 tmap <- newTVarIO stateMap
 tvarFAcid <- newTVarIO fAcidSt
 return $ SimpleCell (CellCore tmap tvarFAcid) ck parentWorkingDir newWorkingDir
  where
      traverseAndWait fp l = do
        aRes <- traverse (traverseLFcn fp) l
        traverse waitCatch aRes
      traverseLFcn  fp fkRaw = (async $ traverseLFcn' fp fkRaw)
      traverseLFcn' fp fkRaw = do 
        let fpKey = fp </> (fromText . (codeCellKeyFilename ck) $ fkRaw) 
        est' <- openCKSt fpKey emptyTargetState
        print $ "opened: " ++ (show fpKey)    
        return $ fmap (\st' -> (fkRaw, st')) est'       



openCKSt :: Serialize st =>
             FilePath -> st -> IO (Either StoreError (SimpleStore st))
openCKSt fpKey _emptyTargetState = openSimpleStore (fpKey)
  
-- -- | Exception and Error handling
-- -- type AEither a = Either StoreCellErrora

