-- | Utility functions

module Control.Monad.Utils where

import Control.Concurrent.Async
import Control.DeepSeq
import Control.Exception
import Data.Monoid

{-# INLINABLE for_ #-}
for_ :: (Num n, Ord n, NFData a) => n -> n -> (n -> IO a) -> IO ()
for_ start end _
  | start > end = error "start greater than end"
for_ start end fn = loop start
  where
    loop i
      | i > end = return ()
      | otherwise = fn i >>= evaluate . rnf >> loop (i + 1)

{-# INLINABLE forConc_ #-}
forConc_ :: (Num n, Ord n, NFData a) => n -> n -> (n -> IO a) -> IO ()
forConc_ start end _
  | start > end = error "start greater than end"
forConc_ start end fn = runConcurrently (loop start)
  where
    loop i
      | i > end = pure ()
      | otherwise = Concurrently (fn i >>= evaluate . rnf) <> loop (i + 1)
