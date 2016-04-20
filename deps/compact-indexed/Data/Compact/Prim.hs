{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE Strict        #-}
{-# LANGUAGE StrictData    #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Another version of indexed compacts

module Data.Compact.Prim where

import           Control.DeepSeq
import           Control.Monad.Primitive
import qualified Data.Compact.Internal   as C
import           GHC.Prim
import           GHC.Types

newtype CIO s a = CIO (State# (Compact s ()) -> (#State# (Compact s ()), a #))
  deriving (Functor, Applicative, Monad)

instance PrimMonad (CIO s) where
  type PrimState (CIO s) = Compact s ()
  primitive :: (State# (Compact s ()) -> (#State# (Compact s ()), a #)) -> CIO s a
  primitive = undefined

data Compact s a = Compact Compact# a

getCompact :: Compact s a -> a
getCompact (Compact _ obj) = obj

appendCompact :: NFData a => Compact s b -> a -> CIO s (Compact s a)
appendCompact = compactAppendInternalIO 1#

compactAppendInternal :: NFData a => Compact# -> a -> Int# ->
                         State# RealWorld -> (# State# RealWorld, Compact s a #)
compactAppendInternal buffer root share s =
  case force root of
    eval -> compactAppendEvaledInternal buffer eval share s

compactAppendInternalIO :: NFData a => Int# -> Compact s b -> a -> CIO s (Compact s a)
compactAppendInternalIO share (Compact buffer _) root =
  IO (\s -> compactAppendInternal buffer root share s)

compactAppendEvaledInternal :: Compact# -> a -> Int# -> State# RealWorld ->
                               (# State# RealWorld, Compact s a #)
compactAppendEvaledInternal buffer root share s =
  case compactAppend# buffer root share s of
    (# s', adjustedRoot #) -> (# s', Compact buffer adjustedRoot #)
