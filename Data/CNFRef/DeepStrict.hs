{-# LANGUAGE FlexibleInstances #-}

-- | A hypothetical typeclass to track transitive strictness.
-- I.e. datatypes whose values cannot contain (reach) a thunk.

module Data.CNFRef.DeepStrict where

import Control.DeepSeq
import Data.IORef
import Data.Vector.Unboxed         (Vector)
import Data.Vector.Unboxed.Mutable (IOVector)
import System.IO.Unsafe

instance NFData a => NFData (IORef a) where
  rnf a = unsafePerformIO $ modifyIORef' a force

-- RRN: Perhaps this implication should go the other way?  Any data
-- that is DeepStrict should trivially have an NFData instance.
class NFData a => DeepStrict a where

instance DeepStrict Int
instance DeepStrict (Vector Int)
instance DeepStrict (IOVector Int)
