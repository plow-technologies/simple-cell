{-# LANGUAGE OverloadedStrings #-}
module SimpleStore.CellSpec (main, spec) where

import Control.Applicative ((<$>))
import Control.Monad
import Data.Maybe (catMaybes)
import Data.Traversable
import SimpleStore
import Test.Hspec
import Test.QuickCheck (arbitrary, Property)
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)
import TestImport

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "serialization Test" $ do
    it "after initializing a value , closing and opening the value should match" $ do
      propInitThenRead


runRestartTest :: [Int] -> IO [Int]
runRestartTest i = do
  let sis = Sample <$> i
  sc <- initializeSampleSC "testSampleCell"
  void $ traverse (insertSampleSC sc ) sis
  createCheckpointAndCloseSampleSC sc
  sc' <- initializeSampleSC "testSampleCell"
  storeSamples <- traverse (getSampleSC sc') sis
  samples <- traverse (traverse getSimpleStore) storeSamples
  return $ sampleInt <$> (catMaybes samples)
 


propInitThenRead :: Property
propInitThenRead = monadicIO $ do i <- pick arbitrary
                                  r <- run $ runRestartTest i
                                  assert $ (r) == i
