{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | A hypothetical typeclass to track transitive strictness.
-- I.e. datatypes whose values cannot contain (reach) a thunk.

module Data.CNFRef.DeepStrict where

import Control.DeepSeq
import Control.Monad
import GHC.Int
import GHC.Prim
import System.IO.Unsafe

import Data.IORef
import Data.Primitive.MutVar
import Data.Vector.Mutable         as V
import Data.Vector.Unboxed.Mutable as U

-- RRN: Perhaps this implication should go the other way?  Any data
-- that is DeepStrict should trivially have an NFData instance.
-- VC: But Compact requires NFData.
-- instance DeepStrict a => NFData a
-- will result in overlapping instances.
-- This can be done in a better way using IntrinsicSuperclasses.
class NFData a => DeepStrict a where

-- DeepStrict instances
instance DeepStrict Int
instance DeepStrict Int64
instance DeepStrict a => DeepStrict (V.IOVector a)
instance DeepStrict a => DeepStrict (U.IOVector a)

-- NFData instances for using compact (debatable)

-- Control.DeepSeq now exports a safe NFData instance for IORef which
-- only evaluates the reference. This seems to work just fine with
-- ghc-8.1, although it used to crash in the CNF implementation
-- earlier.
-- instance NFData a => NFData (IORef a) where
--   rnf a = unsafePerformIO $ modifyIORef' a force

instance NFData a => NFData (MutVar RealWorld a) where
  rnf a = unsafePerformIO $ modifyMutVar' a force

instance NFData a => NFData (V.IOVector a) where
  rnf a = unsafePerformIO $ modifyIOVector' a force
    where
      modifyIOVector' !a !f = go' a f 0 (V.length a)
      go' !a !f !i !l = when (i < l) $
        V.unsafeModify a f i >> go' a f (i + 1) l
