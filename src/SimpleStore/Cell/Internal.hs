{-# LANGUAGE BangPatterns #-}


module SimpleStore.Cell.Internal
    (
    ioFoldRListT
    ) where


import           Control.Concurrent.STM
import qualified ListT
import Control.Monad

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



ioTraverseListT :: ListT.MonadTransUncons t =>
                                          (a -> IO b) ->  
                                          t STM a -> 
                                          IO b
ioTraverseListT fcn stmA = ioTraverseListT' fcn stmA 


ioTraverseListT' fcn stmListT = do 
          (ma,mlst) <- atomically $ do
                         ma <- ListT.head stmListT
                         mlst <- ListT.tail stmListT
                         return (ma,mlst)
          case ma  of
            Nothing -> mzero
            (Just a) -> fcn a

