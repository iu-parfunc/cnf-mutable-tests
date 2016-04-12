{-# LANGUAGE Strict #-}

module Utils where

{-# INLINABLE for_ #-}
for_ :: (Num n, Ord n, Monad m) => n -> n -> (n -> m a) -> m ()
for_ start end _
  | start > end = error "start greater than end"
for_ start end fn = loop start
  where
    loop i
      | i > end = return ()
      | otherwise = fn i >> loop (i + 1)
