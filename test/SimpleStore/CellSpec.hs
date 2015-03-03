{-# LANGUAGE OverloadedStrings #-}
module SimpleStore.CellSpec (main, spec) where



import SimpleStore
import Test.Hspec
import Test.QuickCheck (arbitrary, Property)
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)
import TestImport
import Control.Applicative ((<$>))
import Data.List (nub)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "serialization Test" $ do
    it "after initializing a value , closing and opening the value should match" $ do
      propInitThenRead
  describe "double-insert-failure Test" $ do
    it "inserting a value as a store twice should fail" $ do
      propDoubleInsert
 



propInitThenRead :: Property
propInitThenRead = monadicIO $ do i <- nub <$> pick arbitrary
                                  r <- run $ runRestartTest i
                                  assert $ (r) == (Right i)

propDoubleInsert :: Property
propDoubleInsert = monadicIO $ do i <- pick arbitrary
                                  r <- run $ runDoubleInsertTest i
                                  assert $ r == (Left StoreDirectoryAlreadyExists)

