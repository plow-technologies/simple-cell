{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>))
import Control.Monad (void)
import qualified Data.Text as T
import Data.Traversable (traverse)
import SimpleStore
import System.Random (getStdGen, randoms)
import TestImport
main :: IO ()
main = do
  sc <- initializeSampleSC "benchTestCell"
  stdGen <- getStdGen 
  let sis = Sample <$> randoms stdGen
  void $ traverse (insertSampleSC sc) sis
  



storeState sc st' = do 
  mSt <- getSampleSC sc st'
  case mSt of
    Nothing -> return st'
    Just st -> do
      eT <- modifySimpleStore st (return)
      eT' <- createCheckpoint st
      _ <- either (\e -> fail (T.unpack $ T.concat [T.pack.show $ e," modify failure"] ) ) (const $ return st') (eT >> eT')
      return st'




