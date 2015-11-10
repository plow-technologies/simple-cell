{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes   #-}


module SimpleStore.Cell.Internal
    (
    ioFoldRListT ,
    ioTraverseListT_ ,
    ioFromList
    ) where


import           Control.Concurrent.STM
import qualified ListT
import qualified STMContainers.Map      as M

import           Data.Foldable
ioFoldRListT :: ListT.MonadTransUncons t =>
       (a -> b -> b) -> b -> t STM a -> IO b
ioFoldRListT fcn !seed lst = ioFoldRListT' fcn (return seed) lst



ioFoldRListT'
  :: ListT.MonadTransUncons t =>
     (a -> b -> b) -> IO b -> t STM a -> IO b
ioFoldRListT' fcn !iseed lst = do
  (ma,mlst) <- atomically $ do
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



ioTraverseListT_ :: ListT.MonadTransUncons t =>
                                           (a -> IO b) ->
                                           t STM a ->
                                           IO ()
ioTraverseListT_ fcn stmA = ioTraverseListT_' fcn stmA


ioTraverseListT_'
  :: ListT.MonadTransUncons t => (a -> IO a1) -> t STM a -> IO ()
ioTraverseListT_' fcn stmListT = do
          (ma, mlst) <- atomically $ do
                          ma <- ListT.head stmListT
                          mlst <- ListT.tail stmListT
                          return (ma,mlst)
          case ma  of
            Nothing -> return ()
            (Just a) -> fcn a >>
                         maybe  (return ())
                                (ioTraverseListT_' fcn   )
                                mlst



ioFromList lst = atomically  createMap
  where
    createMap = do
                   Data.Foldable.foldl' (\stmAccumulatorMap (k,v) -> do
                                                     accumulatorMap <- stmAccumulatorMap
                                                     M.insert v k accumulatorMap
                                                     return accumulatorMap )  (M.new) lst

