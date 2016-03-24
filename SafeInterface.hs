{-# LANGUAGE BangPatterns #-}

-- | Brainstorming file with a few different versions of the API.

module Main where

import Control.DeepSeq
-- import Data.Compact
import Data.IORef
import System.IO.Unsafe
-- import Control.Monad.ST
-- import Data.STRef

newtype CNFRef s a = CNFRef (IORef a)


-- Version 1:
----------------------------------

class DeepStrict a where

-- The set of types that live in a CNF region and have a region
-- variable that tracks their membership.
class Tracked t where
  copy :: DeepStrict (t s1) => t s1 -> t s2 

data MutCNF s a 

instance Functor (MutCNF s)
instance Applicative (MutCNF s)
instance Monad (MutCNF s)

-- newCNFRef :: DeepStrict a => a -> MutCNF s (CNFRef s a)

newCNFRef :: Tracked t => t s -> MutCNF s (CNFRef s (t s))
newCNFRef !a = undefined

writeCNFRef :: Tracked t => CNFRef s (t s) -> t s -> MutCNF s ()
writeCNFRef _ !a = undefined

-- Version 2:
----------------------------------

-- Abstract datatype (hidden constructor)
-- Can only be created with copy2.
data In s a
-- ^ RRN: It probably makes more sense to add an 's' param to
-- `Compact` itself, rather than have `In`.

newCNFRef2 :: (In s a) -> IO (CNFRef s a)
newCNFRef2 !a = undefined

readCNFRef2 :: CNFRef s a -> IO (In s a)
readCNFRef2 = undefined

writeCNFRef2 :: (CNFRef s a) -> (In s a) -> IO ()
writeCNFRef2 = undefined

-- FIXME: how do we know what compact to append to?
copy2 :: DeepStrict a => a -> IO (In s a)
copy2 !a = undefined

open :: In s a -> a 
open = undefined

-- Version 3:
----------------------------------

newCNFRef3 :: DeepStrict a => a -> IO (CNFRef s a)
newCNFRef3 !a = undefined

readCNFRef3 = readCNFRef2

writeCNFRef3 = writeCNFRef2

-- Do a copy then a write:
writeAppend :: DeepStrict a => (CNFRef s a) -> a -> IO ()
writeAppend = undefined

-- Copy a value into the same Compact as the given CNFRef.
-- Don't MUTATE the Ref.
copy3 :: DeepStrict a => CNFRef s b -> a -> IO (In s a)
copy3 _ref !_a = undefined

-- Examples
-----------------------------------

newtype TrackedInt s = TI Int
-- ^ This could just be `Compact s Int`...

data Tree = Leaf !Int |  Node !Tree !Tree

instance DeepStrict Tree
instance DeepStrict Int

test = do i   <- copy2 (3::Int)
          ref <- newCNFRef2 i
          i'  <- copy2 (open i + 1)
          writeCNFRef2 ref i'

test3 = do ref <- newCNFRef3 (3::Int)
           i   <- readCNFRef3 ref
           writeAppend ref (open i + 1)

           i' <- copy3 ref (open i + 1)
           writeCNFRef3 ref i'

