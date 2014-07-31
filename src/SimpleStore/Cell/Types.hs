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
                            , CellKeyStore (..)
                            , SimpleCell (..)
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


-- Component Libraries
import DirectedKeys.Types

-- Containers 
import qualified Data.Map as M 
import qualified Data.Set as S

-- Strings /Monomorphs 
import qualified Data.Text as T

import SimpleStore


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


data StoreCellError  = InsertFail    !Text 
                     | DeleteFail    !Text
                     | StateNotFound !Text

-- queryCellStore :: (QueryEvent event, Ord tm, Ord dst, Ord src, Ord k) =>
--      SimpleCell k src dst tm (EventState event) stdormant
--      -> DirectedKeyRaw k src dst tm -> event -> IO (Maybe (EventResult event))
-- queryCellStore cell key event = do
--   liveMap  <- readTVarIO . ccLive . cellCore $ cell
--   case M.lookup key liveMap of
--     (Just st) ->  $ query' st event >>= return . Just
--     Nothing -> return Nothing

-- updateCellStore :: (UpdateEvent event, Ord tm, Ord dst, Ord src, Ord k) =>
--                          SimpleCell
--                            k
--                            src
--                            dst
--                            tm
--                            (EventState event)
--                            stdormant
--                          -> DirectedKeyRaw k src dst tm
--                          -> event
--                          -> IO
--                               (Maybe (EventResult event))    
-- updateCellStore cell key event = do
--   liveMap <- readTVarIO . ccLive . cellCore $ cell
--   case M.lookup key liveMap of
--     (Just st) -> do
--       let updateInnerFcn = do
--             rslt <- (update' st event) 
--             createCheckpoint st
--             return rslt
--       rslt <-  updateInnerFcn
--       return . Just $ rslt 
--     Nothing -> return Nothing


-- withSimpleStore :: (Ord tm, Ord dst, Ord src, Ord k) =>
--                        SimpleCell k src dst tm stlive stdormant
--                        -> DirectedKeyRaw k src dst tm
--                        -> (SimpleStore stlive -> IO a)
--                        -> IO (Maybe a)
-- withSimpleStore cell key func = do
--   liveMap <- readTVarIO . ccLive . cellCore $ cell
--   case M.lookup key liveMap of
--     (Just st) -> do
--       rslt <- func st
--       return . Just $ rslt
--     Nothing -> return Nothing
