{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}

{-# LANGUAGE Safe #-}

-- module Main where

import Data.IORef
import GHC.Generics
-- import Control.Monad.ST
-- import Data.STRef
import Data.Proxy
import GHC.TypeLits

newtype CNFRef s a = CNFRef (IORef a)

class GDeepStrict (f :: * -> *) where
 -- no methods

-- class (Generic a, GDeepStrict (Rep a)) => DeepStrict (a :: *) where

class DeepStrict (a :: *) where
  dummy :: Proxy a
  default dummy :: (Generic a, GDeepStrict (Rep a)) => Proxy a
  dummy = Proxy

-- instance (Generic a, GDeepStrict (Rep a)) => DeepStrict (a :: *)

instance DeepStrict Int where
  dummy = Proxy

--------------------------------------

data Tree = Leaf !Int |  Node !Tree !Tree
  deriving (Generic, DeepStrict)

data LazyTree = LLeaf !Int | LNode LazyTree LazyTree
  deriving (Generic, DeepStrict)
-- The above line produces this error:
     -- No instance for (GDeepStrict
     --                     (M1
     --                        S
     --                        ('MetaSel
     --                           'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
     --                        (Rec0 LazyTree)))
     --    arising from a use of ‘Main.$dmdummy’

-- We don't export the symbol "dummy" to prevent this:
-- instance DeepStrict LazyTree where
--   dummy = Proxy

instance GDeepStrict U1
instance GDeepStrict V1
instance GDeepStrict f => GDeepStrict (D1 m f)
instance GDeepStrict f => GDeepStrict (C1 m f)
instance (GDeepStrict f, GDeepStrict g) => GDeepStrict (f :+: g)
instance (GDeepStrict f, GDeepStrict g) => GDeepStrict (f :*: g)
instance GDeepStrict (Rec0 f)

-- This instance needs to assert that the strictness bits are checked:
instance GDeepStrict f => GDeepStrict (S1 ('MetaSel mb su ss 'DecidedStrict) (f :: * -> *))
  -- The super class is considered REDUNDANT.. but that's bogus!!!
instance GDeepStrict f => GDeepStrict (S1 ('MetaSel mb su ss 'DecidedUnpack) (f :: * -> *))

-- This approach would work, but it would not allow us to use SafeHaskell:
--   $(makeDeepStrictInstance 'Tree)
--   $(makeDeepStrictInstance 'LazyTree)

-- This claims to need undecidable instances, huh?
-- instance (TypeError ('Text "Cannot derive DeepStrict; lazy field found: " :<>: 'ShowType f), GDeepStrict f)
instance (TypeError ('Text "Cannot derive DeepStrict; lazy field found."), GDeepStrict f)
   => GDeepStrict (S1 ('MetaSel mb su ss DecidedLazy) f)

-- TODO: prod types, sum types, constant args, unlifted types

-- instance DeepStrict Tree
--instance DeepStrict Int


