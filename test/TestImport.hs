{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module TestImport where

import           Data.Aeson
import           GHC.Generics

import           Data.Serialize
import           Data.Aeson.Serialize

import Control.Applicative ((<$>))
import Data.Traversable (traverse)
import Data.Maybe (catMaybes)
import Control.Monad (void)
import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)
import Data.Hashable
-- Needed for store creation
import           SimpleStore
import           SimpleStore.Cell
import           DirectedKeys
import           DirectedKeys.Types
-- import DirectedKeys.Router
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Filesystem (removeTree)








data Sample = Sample {
           sampleInt :: Int 
           } 
    deriving (Show,Eq,Generic)





newtype SampleDst = SampleDst { unSampleDst :: Int }
 deriving (Eq,Ord,Generic,Hashable) 

instance Serialize SampleDst where


  
newtype SampleSrc = SampleSrc { unSampleSrc :: Int }
 deriving (Eq,Ord,Generic ,Hashable) 

instance Serialize SampleSrc where



newtype SampleKey = SampleKey { unSampleKey :: Int } 
 deriving (Eq,Ord,Generic ,Hashable) 

instance Serialize SampleKey where


newtype SampleTime = SampleTime { unSampleTime :: Int }
 deriving (Eq,Ord,Generic ,Hashable) 

instance Serialize SampleTime where



----------------------------


sampleSrc :: SampleSrc
sampleSrc = SampleSrc 1

sampleDst :: SampleDst
sampleDst = SampleDst 1

sampleTime :: SampleTime
sampleTime = (SampleTime 0)



instance ToJSON Sample where 
instance FromJSON Sample where 

instance Serialize Sample where  
  get = getFromJSON
  put = putToJSON

instance SimpleCellState Sample where
  type SimpleCellKey      Sample = SampleKey
  type SimpleCellSrc      Sample = SampleSrc
  type SimpleCellDst      Sample = SampleDst
  type SimpleCellDateTime Sample = SampleTime
  simpleCellKey = sampleStoreCellKey

initSample :: Sample
initSample = Sample 0 

sampleStoreCellKey :: CellKey SampleKey SampleSrc SampleDst SampleTime Sample 
sampleStoreCellKey =  CellKey { getKey = getKeyFcn 
                              , codeCellKeyFilename = fullEncodeFcn
                              , decodeCellKeyFilename = fullDecodeFcn
                              }



type DirectedSampleKey =   DirectedKeyRaw  SampleKey SampleSrc SampleDst SampleTime
type SampleCell = SimpleCell
                       SampleKey
                       SampleSrc
                       SampleDst
                       SampleTime
                       Sample
                       (SimpleStore CellKeyStore)

type SampleCK = CellKey SampleKey SampleSrc SampleDst SampleTime Sample

fullEncodeFcn :: DirectedSampleKey -> T.Text
fullEncodeFcn = TE.decodeUtf8 . encodeKey

fullDecodeFcn :: (Serialize datetime, Serialize destination, Serialize source,
                        Serialize key) =>
                       T.Text
                       -> Either T.Text (DirectedKeyRaw key source destination datetime)
fullDecodeFcn akey = case (decodeKey $ TE.encodeUtf8 $ akey) of
                       Left e -> Left . T.pack $ e
                       Right r -> Right r

getKeyFcn :: Sample -> DirectedSampleKey
getKeyFcn st = DKeyRaw (SampleKey . sampleInt $ st) sampleSrc sampleDst sampleTime


runRestartTest :: [Int] -> IO (Either StoreError [Int])
runRestartTest i = runEitherT $ do
  liftIO $ removeTree "testSampleCell" 
  let sis = Sample <$> i
  sc <- EitherT $ initializeSimpleCell initSample "testSampleCell"
  void $ traverse (EitherT . insertStore sc ) sis
  void $ liftIO $ createCellCheckPointAndClose sc
  sc' <- EitherT $ initializeSimpleCell initSample "testSampleCell"
  storeSamples <- liftIO $ traverse (getStore sc') sis
  samples <- liftIO $ traverse (traverse getSimpleStore) storeSamples
  void $ liftIO $ createCellCheckPointAndClose sc
  return $ sampleInt <$> (catMaybes samples)

  
runDoubleInsertTest :: Int -> IO (Either StoreError Int)
runDoubleInsertTest i = runEitherT $ do
  liftIO $ removeTree "testSampleCell" 
  let sampleInt = Sample i
  testSampleCell <- EitherT $ initializeSimpleCell initSample "testSampleCell"
  void $ EitherT $ insertStore testSampleCell sampleInt
  void $ EitherT $ insertStore testSampleCell sampleInt
  return i
