{-# LANGUAGE OverloadedStrings, NoImplicitPrelude,TemplateHaskell, GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# LANGUAGE RecordWildCards, TypeFamilies,DeriveDataTypeable,ScopedTypeVariables #-}

{-| 

    This module defines the types used in the Template haskell routine in order to automate the creation of a
    higher level set of access functions to the Atomic Data in SimpleStore State.

    Most notably, it allows datatypes that look like Keyed vectors to make changes without write locking above the Key Level

    This is very important when writing to Time Series Data.  
    
|-} 



module SimpleStore.Cell.Types (StoreCellError(..)
                            , CellKey (..)
                            , CellKeyStore
                            , CellStore
                            , SimpleCell
                            , DeleteSimpleCellPathFileKey
                            , InsertSimpleCellPathFileKey
                            , initializeSimpleCell
                            , insertStore
                            , updateStore
                            , deleteStore
                            , storeFoldlWithKey
                            , storeTraverseWithKey
                            , createCellCheckPointAndClose
                            , archiveAndHandle 
                            , queryCellStore
                            , updateCellStore
                            , CellStore
                            , stateExists
                            , withSimpleStore
                            ) where


-- System 
import Filesystem.Path.CurrentOS hiding (root)
import Filesystem 

-- Controls
import Prelude (show, (++) )
import CorePrelude hiding (try,catch, finally)
import Control.Concurrent.STM
import Control.Monad.Reader ( ask )
import Control.Monad.State  

import Control.Concurrent.Async
import Control.Exception

-- Typeclasses

import Data.Foldable
import Data.Traversable

import GHC.Generics
import Data.SafeCopy        (SafeCopy,base, deriveSafeCopy)

-- Component Libraries
import DirectedKeys.Types

-- Containers 
import qualified Data.Map as M 
import qualified Data.Set as S

-- Strings /Monomorphs 
import qualified Data.Text as T

-- | 'CellKey' declares two functions, 
-- 'getKey' which is supposed to take a SimpleStore Value and return a 'DirectedKeyRaw' 
-- 'makeFilename' which takes a directed key and returns text.  
-- you could use 'parseFilename' which escapes all the characters with valid unix ones
-- or you can write your own, but it is very important that they are invertable!
-- so be careful


-- | 'CellKey' enforces no guarantees on your filenames, this is done because
-- we want to keep the libraries light and portable but it means you need to add these 
-- guarantees on your functions 

data CellKey k src dst tm st = CellKey { getKey :: st -> (DirectedKeyRaw k src dst tm)
                                  , codeCellKeyFilename :: (DirectedKeyRaw  k src dst tm) -> Text
                                  , decodeCellKeyFilename :: Text -> Either Text (DirectedKeyRaw  k src dst tm)
                                  }

-- | the codeCellKeyFilename actually will be used to make these file keys but
-- I figure why not just make it where you can pass a Text generator.

newtype FileKey = FileKey { getFileKey :: Text} deriving (Show,Generic,Ord,Eq)

  
makeFileKey :: CellKey k src dst tm st -> st -> FileKey 
makeFileKey ck s = FileKey (codeCellKeyFilename ck . getKey ck $ s)

unmakeFileKey :: CellKey k src dst tm st
                       -> FileKey -> Either Text (DirectedKeyRaw k src dst tm)
unmakeFileKey ck s = (decodeCellKeyFilename ck).getFileKey $ s

-- |'CellCoreLive' and 'CellCoreDormant' both define maps to acid states
-- Live means currently loaded into memory
-- Dormant means currently not loaded

data CellCore  k src dst tm tvlive stdormant = CellCore { 
      ccLive     :: ! (TVar (M.Map (DirectedKeyRaw  k src dst tm) tvlive ))
      ,ccDormant :: !(TVar stdormant)
    }



newtype CellKeyStore  = CellKeyStore { getCellKeyStore :: (S.Set FileKey)}
    deriving (Show,Generic)

emptyCellKeyStore :: CellKeyStore
emptyCellKeyStore = CellKeyStore S.empty


-- | Transactional Cell Core
--  Transactional Cell Core is where both the map to live acidstates are stored and the map for 
--  dormant filenames

type TCellCore  k src dst tm stlive stdormant =  (CellCore  k src dst tm  stlive stdormant)


data SimpleCell  k src dst tm stlive stdormant = SimpleCell { 
      cellCore :: !(TCellCore  k src dst tm (SimpleStore stlive) stdormant )
    , cellKey  :: !(CellKey  k src dst tm stlive)
    , cellParentFP :: !FilePath -- /root/otherstuff/parentofopenlocalstatefromdir
    , cellRootFP :: !FilePath -- /root/otherstuff/parentofopenlocalstatefromdir/openLocalStateFromdir
    } 
   deriving (Typeable,Generic)

-- |Cell Core interaction functions 
-- The cell core is designed to be private... These accessors are used for other functions
-- These Functions will be made into the Simple Core


-- | Path manipulation happens at every atomic change to the Cell
-- These functions are made acidic 
-- They do not actually do the deletion and creation of a filepath but instead delete and create the reference to it

-- | DIG FileKey interface is where the acidFunctions live They are functions of fileKey without the conversions

deleteSimpleCellPathFileKey :: SimpleStore st -> FileKey -> IO ( CellKeyStore FileKey )
deleteSimpleCellPathFileKey st fk = do 
  (CellKeyStore { getCellKeyStore = hsSet}) <- getSimpleStore st
  (void $ putSimpleStore st (CellKeyStore (S.delete fk hsSet )))
  return fk


-- |Note... This insert is repsert functional
insertSimpleCellPathFileKey :: SimpleStore st -> FileKey ->  IO ( CellKeyStore FileKey)
insertSimpleCellPathFileKey st fk =  do 
  (CellKeyStore { getCellKeyStore = hsSet}) <- geSimpleStore st
  void $ putSimpleStore st  $ (CellKeyStore (S.insert  fk hsSet ))
  return fk


getSimpleCellPathFileKey :: SimpleStore st -> IO (CellKeyStore ((S.Set FileKey)))
getSimpleCellPathFileKey st = do
  (CellKeyStore { getCellKeyStore = hsSet}) <- getSimpleStore st
  case hsSet of 
    _  
       | S.null hsSet -> return  S.empty
       | otherwise -> do 
              return  hsSet
  
 

-- | User Interface Defining Functions

-- | The 'st' in the type definition here is the AcidState that will be turned into a watched state

-- | Warning, inserting a state that is already inserted throws an exception 

insertStore :: (Ord k, Ord src, Ord dst, Ord tm, IsAcidic t,IsAcidic st) =>
                     CellKey k src dst tm st
                     -> t
                     -> SimpleCell k src dst tm st (SimpleStore st)
                     -> st
                     -> IO (SimpleStore st)
insertStore ck  initialTargetState (SimpleCell (CellCore tlive tvarFStore) _ pdir rdir)  st = do
  let newStatePath = (codeCellKeyFilename ck).(getKey ck) $ st
  fullStatePath <- makeWorkingStatePath pdir rdir newStatePath
  fStore <- readTVarIO tvarFStore
  void $ insertSimpleCellPath ck fStore  st  
  eAcidSt <- (makeSimpleStore (encodeString fullStatePath) st )
  case eAcidSt of
    Left e -> fail e
    Right st -> do 
      atomically (stmInsert acidSt)                                
      atomically $ writeTVar tvarFStore fStore
      return acidSt 
     where 
       stmInsert st' = do 
         liveMap <- readTVar tlive        
         writeTVar tlive $ M.insert (getKey ck st) st' liveMap



makeWorkingStatePath pdir rdir nsp = do 
    void $ when (nsp == "") (fail "--> Cell key led to empty state path")
    return $ pdir </> rdir </> (fromText nsp)

updateStore ck  initialTargetState (SimpleCell (CellCore tlive tvarFStore) _ _pdir _rdir )  simpleSt st = do
  atomically $ stmInsert simpleSt
   where 
     stmInsert st' = do 
       liveMap <- readTVar tlive        
       writeTVar tlive $ M.insert (getKey ck st) st' liveMap

deleteStore :: (Ord k, Ord src, Ord dst, Ord tm) =>
                     CellKey k src dst tm st
                     -> SimpleCell
                          k src dst tm t (AcidState (EventState DeleteSimpleCellPathFileKey))
                     -> st
                     -> IO ()
deleteStore ck (SimpleCell (CellCore tlive tvarFStore) _ pdir rdir) st = do 
  let targetStatePath = (codeCellKeyFilename ck).(getKey ck) $ st 
  void $ atomically stmDelete
  fStore <- readTVarIO tvarFStore
  void $ deleteSimpleCellPath ck fStore st  
  createCheckpoint fStore
  atomically $ writeTVar tvarFStore fStore
  np <- (makeWorkingStatePath pdir rdir targetStatePath)
  removeTree np
     where
        stmDelete = do 
          liveMap <- readTVar tlive
          writeTVar tlive $ M.delete (getKey ck st) liveMap

storeFoldlWithKey :: t6   -> SimpleCell t t1 t2 t3 t4 t5
                           -> (t6
                               -> DirectedKeyRaw t t1 t2 t3 -> SimpleStore t4 -> IO b -> IO b)
                           -> IO b
                           -> IO b

storeFoldlWithKey ck (SimpleCell (CellCore tlive _) _ _ _) fldFcn seed = do 
  liveMap <- readTVarIO tlive 
  M.foldWithKey (\key simpleSt b -> do
                             st <- getSimpleStore simpleSt
                             fldFcn ck key st  b) seed liveMap


storeTraverseWithKey :: forall t t1 t2 t3 t4 t5 t6 b.
                         t6
                         -> SimpleCell t t1 t2 t3 t4 t5
                         -> (t6 -> DirectedKeyRaw t t1 t2 t3 -> SimpleStore t4 -> IO b)
                         -> IO (Map (DirectedKeyRaw t t1 t2 t3) b)
storeTraverseWithKey ck (SimpleCell (CellCore tlive _) _ _ _) tvFcn  = do 
  liveMap <- readTVarIO tlive 
  M.traverseWithKey (\key cs -> lockFunctionIO (getWriteLock cs) $ tvFcnWrp key $ getAcidState cs)  liveMap
      where
        tvFcnWrp k a = do
          ( tvFcn ck k a) -- (\e -> either (\e' -> print e') (\_ -> return () ) e )
          
          
createCellCheckPointAndClose :: forall t t1 t2 t3 t4 st st1.
                                (SafeCopy st1, Typeable st1) =>
                                t -> SimpleCell t1 t2 t3 t4 st (SimpleStore st1) -> IO ()
createCellCheckPointAndClose _ (SimpleCell (CellCore tlive tvarFStore) _ _pdir _rdir ) = do 
  liveMap <- readTVarIO tlive 
  void $ traverse (\st -> (closeSimpleStore . getAcidState $ st)   ) liveMap
  fStore <- readTVarIO tvarFStore
  void $ createCheckpointAndClose fStore

initializeSimpleCell :: (Ord k, Ord src, Ord dst, Ord tm, IsAcidic stlive) =>
                            CellKey k src dst tm stlive
                            -> stlive
                            -> Text -> IO (SimpleCell k src dst tm stlive (SimpleStore CellKeyStore))
initializeSimpleCell ck emptyTargetState root = do 
 parentWorkingDir <- getWorkingDirectory
 let acidRootPath = fromText root
     newWorkingDir = acidRootPath
     fpr           = (parentWorkingDir </> acidRootPath)

 fAcidSt <- openLocalStateFrom (encodeString fpr ) emptyCellKeyStore 

 fkSet   <-   query' fAcidSt (GetSimpleCellPathFileKey)

 let setEitherFileKeyRaw = S.map (unmakeFileKey ck) fkSet  
 let groupedList = groupUp 16 (rights . S.toList $ setEitherFileKeyRaw)
 aStateList <- traverse (traverseAndWait fpr) groupedList
 stateList <- Data.Traversable.sequence $ (fmap addTMVar $ rights . rights $ (Data.Foldable.concat aStateList))
 let stateMap = M.fromList stateList
 tmap <- newTVarIO stateMap
 tvarFAcid <- newTVarIO fAcidSt
 return $ SimpleCell (CellCore tmap tvarFAcid) ck parentWorkingDir newWorkingDir
    where
      addTMVar (k,s) = do
        tv <- newTMVarIO WriteLockST
        return (k,(CellStore tv s))

      traverseAndWait f l = do
        aRes <- traverse (traverseLFcn f) l
        traverse waitCatch aRes
      traverseLFcn  r fkRaw = (async $ traverseLFcn' r fkRaw)
      traverseLFcn' r fkRaw = do 
        let fpKey = r </> (fromText . (codeCellKeyFilename ck) $ fkRaw) 
        est' <- openCKSt fpKey emptyTargetState
        print $ "opened: " ++ (show fpKey)    
        return $ fmap (\st' -> (fkRaw, st')) est'       



openCKSt :: IsAcidic st =>
             FilePath -> st -> IO (Either SomeException (SimpleStore st))
openCKSt fpKey emptyTargetState = try $ openLocalStateFrom (encodeString fpKey) emptyTargetState 
  
-- | Exception and Error handling
-- type AEither a = Either StoreCellErrora

data StoreCellError  = InsertFail    !Text 
                     | DeleteFail    !Text
                     | StateNotFound !Text

queryCellStore :: (QueryEvent event, Ord tm, Ord dst, Ord src, Ord k) =>
     SimpleCell k src dst tm (EventState event) stdormant
     -> DirectedKeyRaw k src dst tm -> event -> IO (Maybe (EventResult event))
queryCellStore cell key event = do
  liveMap  <- readTVarIO . ccLive . cellCore $ cell
  case M.lookup key liveMap of
    (Just (CellStore lock st)) -> lockFunctionIO lock $ query' st event >>= return . Just
    Nothing -> return Nothing

updateCellStore :: (UpdateEvent event, Ord tm, Ord dst, Ord src, Ord k) =>
                         SimpleCell
                           k
                           src
                           dst
                           tm
                           (EventState event)
                           stdormant
                         -> DirectedKeyRaw k src dst tm
                         -> event
                         -> IO
                              (Maybe (EventResult event))    
updateCellStore cell key event = do
  liveMap <- readTVarIO . ccLive . cellCore $ cell
  case M.lookup key liveMap of
    (Just cst@(CellStore lock st)) -> do
      let updateInnerFcn = do
            rslt <- (update' st event) 
            createCheckpoint st
            return rslt
      rslt <- (lockFunctionIO lock $ updateInnerFcn)      
      return . Just $ rslt 
    Nothing -> return Nothing


withSimpleStore :: (Ord tm, Ord dst, Ord src, Ord k) =>
                       SimpleCell k src dst tm stlive stdormant
                       -> DirectedKeyRaw k src dst tm
                       -> (SimpleStore stlive -> IO a)
                       -> IO (Maybe a)
withSimpleStore cell key func = do
  liveMap <- readTVarIO . ccLive . cellCore $ cell
  case M.lookup key liveMap of
    (Just cst@(CellStore lock st)) -> do
      rslt <- lockFunctionIO lock $ func st
      return . Just $ rslt
    Nothing -> return Nothing
