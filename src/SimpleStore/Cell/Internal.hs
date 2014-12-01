{-# LANGUAGE BangPatterns #-}


module SimpleStore.Cell.Internal
    (
    ioFoldRListT
    ) where


import           Control.Concurrent.STM
import qualified ListT


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

