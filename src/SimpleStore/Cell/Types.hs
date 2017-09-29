{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

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
                            , CellCore (..)
                            , FileKey (..)
                            , InitializedCell(..)
                            ) where


-- System
import           Filesystem.Path.CurrentOS (FilePath)


-- Controls

import           Control.Concurrent.STM
import           CorePrelude               (Either, Eq, Ord, Show, Text,
                                            Typeable)






-- Typeclasses




import           GHC.Generics


-- Component Libraries
import           DirectedKeys.Types

-- Containers
-- import qualified Data.Map.Strict as M
import qualified Data.Set                  as S
import qualified STMContainers.Map         as M

-- Strings /Monomorphs


import           SimpleStore

import           Data.Aeson
import           Data.Aeson.Serialize
import           Data.Serialize


-- | 'CellKey' declares two functions,
-- 'getKey' which is supposed to take a SimpleStore Value and return a 'DirectedKeyRaw'
-- 'makeFilename' which takes a directed key and returns text.
-- you could use 'parseFilename' which escapes all the characters with valid unix ones
-- or you can write your own, but it is very important that they are invertable!
-- so be careful


-- | 'CellKey' enforces no guarantees on your filenames, this is done because
-- we want to keep the libraries light and portable but it means you need to add these
-- guarantees on your functions

data CellKey k src dst tm st = CellKey { getKey           :: st -> (DirectedKeyRaw k src dst tm)
                                  , codeCellKeyFilename   :: (DirectedKeyRaw  k src dst tm) -> Text
                                  , decodeCellKeyFilename :: Text -> Either Text (DirectedKeyRaw  k src dst tm)
                                  }

-- | the codeCellKeyFilename actually will be used to make these file keys but
-- I figure why not just make it where you can pass a Text generator.

newtype FileKey = FileKey { getFileKey :: Text} deriving (Show,Generic,Ord,Eq)

instance ToJSON FileKey where
instance FromJSON FileKey where



instance Serialize FileKey where
  get = getFromJSON
  put = putToJSON


-- |'CellCoreLive' and 'CellCoreDormant' both define maps to acid states
-- Live means currently loaded into memory
-- Dormant means currently not loaded

data CellCore  k src dst tm tvlive stdormant = CellCore {
      ccLive     ::  ((M.Map (DirectedKeyRaw  k src dst tm) tvlive ))
      ,ccDormant ::  (TVar stdormant)
    }



newtype CellKeyStore  = CellKeyStore { getCellKeyStore :: (S.Set FileKey)}
    deriving (Show,Generic)

instance ToJSON CellKeyStore where
instance FromJSON CellKeyStore where

instance Serialize CellKeyStore where
  get = getFromJSON
  put = putToJSON



-- | Transactional Cell Core
--  Transactional Cell Core is where both the map to live acidstates are stored and the map for
--  dormant filenames

type TCellCore  k src dst tm stlive stdormant =  (CellCore  k src dst tm  stlive stdormant)


data SimpleCell  k src dst tm stlive stdormant = SimpleCell {
      cellCore     :: !(TCellCore  k src dst tm (SimpleStore stlive) stdormant )
    , cellKey      :: !(CellKey  k src dst tm stlive)
    , cellParentFP :: !FilePath -- /root/otherstuff/parentofopenlocalstatefromdir
    , cellRootFP   :: !FilePath -- /root/otherstuff/parentofopenlocalstatefromdir/openLocalStateFromdir
    }
   deriving (Typeable,Generic)


data StoreCellError  = InsertFail    !Text
                     | DeleteFail    !Text
                     | StateNotFound !Text


data InitializedCell k src dst tm stlive stdormant = InitializedCell
  { initializedCell       :: SimpleCell k src dst tm stlive stdormant
  , initializedCellErrors :: [StoreError]
  } deriving (Typeable, Generic)
