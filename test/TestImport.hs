{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TestImport where

import           Data.Aeson
import           GHC.Generics
import           Data.Serialize
import           Data.Aeson.Serialize
import           Control.Applicative        ((<$>))
import           Data.Traversable           (traverse)
import           Data.Maybe                 (catMaybes)
import           Control.Monad              (void)
import           Data.Hashable
-- Needed for store creation
import           SimpleStore
import           SimpleStore.Cell
import           DirectedKeys
import           DirectedKeys.Types
-- import DirectedKeys.Router
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE


type SampleCell        = SimpleCell     SampleKey SampleSrc SampleDst SampleTime Sample (SimpleStore CellKeyStore)
type SampleCellKey     = CellKey        SampleKey SampleSrc SampleDst SampleTime Sample
type SampleDirectedKey = DirectedKeyRaw SampleKey SampleSrc SampleDst SampleTime

data Sample = Sample {
  sampleInt :: Int 
  } deriving (Show,Eq,Generic)

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

initSample :: Sample
initSample = Sample 0 

sampleStoreCellKey :: SampleCellKey
sampleStoreCellKey =  CellKey { getKey = getKeyFcn 
                              , codeCellKeyFilename = fullEncodeFcn
                              , decodeCellKeyFilename = fullDecodeFcn
                              }

{-
type DirectedSampleKey =   DirectedKeyRaw  SampleKey SampleSrc SampleDst SampleTime
type SampleCell = SimpleCell
                       SampleKey
                       SampleSrc
                       SampleDst
                       SampleTime
                       Sample
                       (SimpleStore CellKeyStore)

type SampleCK = CellKey SampleKey SampleSrc SampleDst SampleTime Sample
-}
fullEncodeFcn :: SampleDirectedKey -> T.Text
fullEncodeFcn = TE.decodeUtf8 . encodeKey

fullDecodeFcn :: ( Serialize datetime
                 , Serialize destination
                 , Serialize source
                 , Serialize key)
                 => T.Text
                 -> Either T.Text (DirectedKeyRaw key source destination datetime)
fullDecodeFcn akey = case (decodeKey $ TE.encodeUtf8 $ akey) of
                       Left e -> Left . T.pack $ e
                       Right r -> Right r

getKeyFcn :: Sample -> SampleDirectedKey
getKeyFcn st = DKeyRaw (SampleKey . sampleInt $ st) sampleSrc sampleDst sampleTime

--- Simple Cell generation

$(makeStoreCell 'sampleStoreCellKey 'initSample ''Sample)

-- | TH Defnitions

getSampleSC :: SampleCell -> SampleDirectedKey -> IO (Maybe (SimpleStore Sample))

getSamplesSC :: SampleCell -> IO [SimpleStore Sample]

updateSampleSC :: SampleCell -> SimpleStore Sample -> Sample -> IO ()

createCheckpointAndCloseSampleSC :: SampleCell -> IO ()
                                          

insertSampleSC :: SampleCell -> Sample -> IO (SimpleStore Sample)

deleteSampleSC :: SampleCell -> SampleDirectedKey -> IO ()                                  

traverseWithKeySampleSC_ 
  :: SampleCell
  -> (SampleCellKey -> SampleDirectedKey -> Sample -> IO ())
  -> IO ()

foldlWithKeySampleSC 
  :: SampleCell
  -> (SampleCellKey -> SampleDirectedKey -> Sample -> IO b -> IO b)
  -> IO b
  -> IO b

initializeSampleSC :: T.Text -> IO SampleCell

getOrInsertSampleSC
  :: SampleCell
  -> Sample 
  -> IO (SimpleStore Sample)
getOrInsertSampleSC sc si = do
  maybeVal <- getSampleSC sc $ getKeyFcn si
  case maybeVal of
    (Just st) -> createCheckpoint st >> return st
    Nothing -> insertSampleSC sc si >>= (\st -> createCheckpoint st >> return st)

runRestartTest :: [Int] -> IO ([Int], Bool)
runRestartTest i = do
  let sis = Sample <$> i
  
  putStrLn "init first time"
  sc <- initializeSampleSC "testSampleCell"
  
  putStrLn "traverse given list"
  void $ traverse (getOrInsertSampleSC sc) sis
  
  putStrLn "first checkpiont and close"
  createCheckpointAndCloseSampleSC sc
  
  putStrLn "init second time"
  sc' <- initializeSampleSC "testSampleCell"
  
  putStrLn "list em"
  storeSamples <- traverse (getSampleSC sc' . getKeyFcn) sis
  
  putStrLn "store em"
  samples <- traverse (traverse getSimpleStore) storeSamples
  
  putStrLn "list all samples in the cell"
  allStoreSamples <- getSamplesSC sc'
  allSamples <- traverse getSimpleStore allStoreSamples
  
  putStrLn "check if all elems of samples are in allSamples"
  let samplesInAllSamples = and $ (flip elem allSamples) <$> (catMaybes samples)
  
  putStrLn "checkpoint"
  createCheckpointAndCloseSampleSC sc'
  
  return $ (sampleInt <$> (catMaybes samples), samplesInAllSamples)


  
