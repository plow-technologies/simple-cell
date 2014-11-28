{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
import Control.Applicative ((<$>))
import Control.Monad (void,forever)
import qualified Data.Text as T
import Data.Traversable (traverse)
import SimpleStore
import System.Random (getStdGen, randoms)
import Control.Concurrent (threadDelay)
import TestImport
       
main :: IO ()
main = do
  sc <- initializeSampleSC "benchTestCell"
  stdGen <- getStdGen 
  let sis = take 5000 $ Sample <$> randoms stdGen
  void $ traverse (insertSampleSC sc) sis
  forever $ do
    void $ traverse (storeState sc) sis




storeState sc st' = do
  putStrLn "running"
  threadDelay $ 3*1000  
  mSt <- getSampleSC sc st'
  case mSt of
    Nothing -> return st'
    Just st -> do
      eT <- modifySimpleStore st (return)
      eT' <- createCheckpoint st
      _ <- either (\e -> fail (T.unpack $ T.concat [T.pack.show $ e," modify failure"] ) ) (const $ return st') (eT >> eT')
      return st'




