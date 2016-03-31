{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}

-- | A hypothetical typeclass to track transitive strictness.
-- I.e. datatypes whose values cannot contain (reach) a thunk.

module Data.CNFRef.DeepStrict where

import Control.DeepSeq
import Control.Monad
import GHC.Prim
import System.IO.Unsafe

import Data.IORef
import Data.Primitive.MutVar
import Data.Vector.Mutable         as V
import Data.Vector.Unboxed.Mutable as U

-- RRN: Perhaps this implication should go the other way?  Any data
-- that is DeepStrict should trivially have an NFData instance.
class NFData a => DeepStrict a where

-- DeepStrict instances
instance DeepStrict Int
instance DeepStrict a => DeepStrict (V.IOVector a)
instance DeepStrict a => DeepStrict (U.IOVector a)

-- NFData instances for using compact (debatable)
instance NFData a => NFData (IORef a) where
  rnf a = unsafePerformIO $ modifyIORef' a force

instance NFData a => NFData (MutVar RealWorld a) where
  rnf a = unsafePerformIO $ modifyMutVar' a force

instance NFData a => NFData (V.IOVector a) where
  rnf a = unsafePerformIO $ modifyIOVector' a force
    where
      modifyIOVector' !a !f = go' a f 0 (V.length a)
      go' !a !f !i !l = when (i < l) $
        V.unsafeModify a f i >> go' a f (i + 1) l
