{-# LANGUAGE Strict #-}

module Utils where

import Control.DeepSeq
import Control.Exception

{-# INLINABLE for_ #-}
for_ :: (Num n, Ord n, NFData a) => n -> n -> (n -> IO a) -> IO ()
for_ start end _
  | start > end = error "start greater than end"
for_ start end fn = loop start
  where
    loop i
      | i > end = return ()
      | otherwise = fn i >>= (evaluate . rnf) >> loop (i + 1)
