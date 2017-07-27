{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bench where
  
import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Monad (void,forever)
import Data.Maybe
import Data.Serialize
import qualified Data.Text as T
import Data.Traversable (traverse)
import SimpleStore
import System.Random (getStdGen, randoms)
import TestImport

main :: IO ()
main = do
  sc <- initializeSampleSC "benchTestCell"
  stdGen <- getStdGen 
  let sis = take 500 $ Sample <$> randoms stdGen
  void $ traverse (insertSampleSC sc) sis
  samples <- traverse (storeState sc) sis
  forever $ do
    print "restart"
    void $ traverse checkpointWithoutReGet  (catMaybes samples)

storeState sc sample = do
 putStrLn "running"
 threadDelay $ 3*1000  
 msimpleStoreSample <- getSampleSC sc sample
 return (msimpleStoreSample >>= 
                \simpleStoreSample -> return (simpleStoreSample, sample) )


checkpointWithoutReGet :: Data.Serialize.Serialize st 
                       => (SimpleStore st, t) 
                       -> IO (SimpleStore st)
checkpointWithoutReGet  (simpleStoreSample,sample) = do
  eT' <- createCheckpoint simpleStoreSample
  a <- either (\e -> fail (T.unpack $ T.concat [T.pack.show $ e," modify failure"] ) ) (const $ return simpleStoreSample) eT' -- (eT >> eT')
  return simpleStoreSample

