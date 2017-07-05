{-# LANGUAGE BangPatterns #-}

module SimpleStore.Cell.Internal ( 
    ioFoldRListT 
  , ioTraverseListT_
  , ioFromList
  ) where

import           Control.Concurrent.STM
import           Data.Foldable
import           Data.Hashable
import           ListT (ListT)
import qualified ListT
import qualified STMContainers.Map as M


ioFoldRListT :: (a -> b -> b) -> b -> ListT STM a -> IO b
ioFoldRListT fcn !seed lst = ioFoldRListT' fcn (return seed) lst

ioFoldRListT' :: (a -> b -> b) -> IO b -> ListT STM a -> IO b 
ioFoldRListT' fcn !iseed lst = do 
  (ma,mlst) <- 
    atomically $ do
      ma <- ListT.head lst 
      mlst <- ListT.tail lst
      return (ma,mlst)
  seed <- iseed                 
  case ma of
   Nothing -> return seed
   Just val ->  do
     maybe (return seed)
           (ioFoldRListT' fcn (return $ fcn val seed))
           mlst                 

ioTraverseListT_ :: (a -> IO b) -> ListT STM a -> IO ()
ioTraverseListT_ fcn stmA = ioTraverseListT_' fcn stmA 


ioTraverseListT_' :: (a -> IO a1) -> ListT STM a -> IO ()
ioTraverseListT_' fcn stmListT = do 
  (ma, mlst) <-
    atomically $ do
      ma <- ListT.head stmListT
      mlst <- ListT.tail stmListT
      return (ma,mlst)
  case ma of
    Nothing  -> return ()
    (Just a) -> 
      fcn a >>
        maybe  (return ())
               (ioTraverseListT_' fcn)
               mlst

ioFromList :: (Hashable k, Foldable t, Eq k) => t (k, v) -> IO (M.Map k v)
ioFromList lst = atomically  createMap 
  where 
    createMap = Data.Foldable.foldl' 
      (\stmAccumulatorMap (k,v) -> do
          accumulatorMap <- stmAccumulatorMap
          M.insert v k accumulatorMap 
          return accumulatorMap) 
      (M.new) lst
