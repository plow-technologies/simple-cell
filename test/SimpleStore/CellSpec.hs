module SimpleStore.CellSpec (main, spec) where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "serialization Test" $ do
    it "should enable insertion and checkpoints while still running" $ do
      True `shouldBe` False



