{-# LANGUAGE OverloadedStrings #-}

module SimpleStore.CellSpec (main, spec) where

import Test.Hspec
import TestImport
import Test.QuickCheck (arbitrary, Property)
import Test.QuickCheck.Monadic (assert, monadicIO, pick, run)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "serialization Test" $ do
    it "after initializing a value, closing and opening the value should match" $ do
      propInitThenRead

propInitThenRead :: Property
propInitThenRead = monadicIO $ do 
  i <- pick arbitrary
  (r,t) <- run $ runRestartTest i
  assert $ (r) == i
  assert $ t == True
