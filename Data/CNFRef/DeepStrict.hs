{-# LANGUAGE FlexibleInstances #-}

-- | A hypothetical typeclass to track transitive strictness.
-- I.e. datatypes whose values cannot contain (reach) a thunk.

module Data.CNFRef.DeepStrict where

import Control.DeepSeq
import Data.IORef
import Data.Vector.Unboxed (Vector)
import System.IO.Unsafe

instance NFData a => NFData (IORef a) where
  rnf a = unsafePerformIO $ modifyIORef' a force

class NFData a => DeepStrict a where

instance DeepStrict Int
instance DeepStrict (Vector Int)