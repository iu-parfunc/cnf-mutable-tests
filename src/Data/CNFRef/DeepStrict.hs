{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Strict             #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | A hypothetical typeclass to track transitive strictness.
-- I.e. datatypes whose values cannot contain (reach) a thunk.

module Data.CNFRef.DeepStrict where

import Control.DeepSeq
import Control.Monad
import Data.Compact.Indexed
import Data.Int
import Data.Word
import GHC.Generics
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

class GDeepStrict (f :: * -> *) where

instance GDeepStrict U1
instance GDeepStrict V1
instance GDeepStrict f => GDeepStrict (D1 m f)
instance GDeepStrict f => GDeepStrict (C1 m f)
instance (GDeepStrict f, GDeepStrict g) => GDeepStrict (f :+: g)
instance (GDeepStrict f, GDeepStrict g) => GDeepStrict (f :*: g)
instance GDeepStrict (Rec0 f)

-- DeepStrict instances

instance DeepStrict Int
instance DeepStrict Word
instance DeepStrict Integer
instance DeepStrict Float
instance DeepStrict Double

instance DeepStrict Char
instance DeepStrict Bool
instance DeepStrict ()

instance DeepStrict Int8
instance DeepStrict Int16
instance DeepStrict Int32
instance DeepStrict Int64

instance DeepStrict Word8
instance DeepStrict Word16
instance DeepStrict Word32
instance DeepStrict Word64

deriving instance DeepStrict a => DeepStrict (Maybe a)
deriving instance DeepStrict a => DeepStrict [a]
deriving instance (DeepStrict a, DeepStrict b) => DeepStrict (Either a b)
deriving instance (DeepStrict a, DeepStrict b) => DeepStrict (a, b)

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
  rnf a = ()

-- instance NFData a => NFData (V.IOVector a) where
--   rnf a = unsafePerformIO $ modifyIOVector' a force
--     where
--       modifyIOVector' v f = go' v f 0 (V.length a)
--       go' v f i l = when (i < l) $
--         V.unsafeModify v f i >> go' v f (i + 1) l

instance NFData a => NFData (Compact s a) where
  rnf _ = ()

instance DeepStrict a => DeepStrict (Compact s a)
