{-# LANGUAGE OverloadedStrings #-}
module SimpleStore.CellSpec (main, spec) where



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



propInitThenRead :: Property
propInitThenRead = monadicIO $ do i <- pick arbitrary
                                  r <- run $ runRestartTest i
                                  assert $ (r) == i
