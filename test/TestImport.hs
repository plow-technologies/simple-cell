{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module TestImport where

import           Data.Aeson
import           GHC.Generics

import           Data.Serialize
import           Data.Aeson.Serialize


-- Needed for store creation
import           SimpleStore
import           SimpleStore.Cell
import           DirectedKeys
import           DirectedKeys.Types
-- import DirectedKeys.Router
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import           Data.Map






data Sample = Sample {
           sampleInt :: Int 
           } 
    deriving (Show,Eq,Generic)





newtype SampleDst = SampleDst { unSampleDst :: Int }
 deriving (Eq,Ord,Generic) 

instance Serialize SampleDst where


  
newtype SampleSrc = SampleSrc { unSampleSrc :: Int }
 deriving (Eq,Ord,Generic) 

instance Serialize SampleSrc where



newtype SampleKey = SampleKey { unSampleKey :: Int } 
 deriving (Eq,Ord,Generic) 

instance Serialize SampleKey where


newtype SampleTime = SampleTime { unSampleTime :: Int }
 deriving (Eq,Ord,Generic) 

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
fullDecodeFcn akey = case (decodeKey $ TE.encodeUtf8 $ akey) of
                       Left e -> Left . T.pack $ e
                       Right r -> Right r

getKeyFcn :: Sample -> DirectedSampleKey
getKeyFcn st = DKeyRaw (SampleKey . sampleInt $ st) sampleSrc sampleDst sampleTime




--- Simple Cell generation

$(makeStoreCell 'sampleStoreCellKey 'initSample ''Sample)


---------------------
getSampleSC :: SampleCell -> Sample -> IO (Maybe (SimpleStore Sample))


updateSampleSC :: SampleCell
                  -> SimpleStore Sample -> Sample -> IO ()


createCheckpointAndCloseSampleSC :: SampleCell 
                                          -> IO ()

-- traverseWithKeySampleSC :: SampleCell -> SampleCK -> DirectedSampleKey -> Sample -> IO b -> IO (Map DirectedSampleKey b)


traverseWithKeySampleSC :: SampleCell
                                 -> (SampleCK
                                     -> DirectedSampleKey -> Sample -> IO b)
                                 -> IO (Map (DirectedSampleKey) b)


foldlWithKeySampleSC :: SampleCell
                              -> (SampleCK
                                  -> DirectedSampleKey ->Sample -> IO b -> IO b)
                              -> IO b
                              -> IO b
deleteSampleSC :: SimpleCell
                          SampleKey
                          SampleSrc
                          SampleDst
                          SampleTime
                          t
                          (SimpleStore CellKeyStore)
                        -> Sample -> IO ()

insertSampleSC :: SampleCell
                        -> Sample -> IO (SimpleStore Sample)

initializeSampleSC :: T.Text
                            -> IO SampleCell
                                 
