{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE Strict        #-}
{-# LANGUAGE StrictData    #-}

module Data.CNFRef where
       -- ( CNFRef(..) -- Transparent for now...
       -- , copyToCompact
       -- , newCNFRef
       -- , appendCNFRef
       -- , readCNFRef
       -- , writeCNFRef
       -- -- , modifyCNFRef
       -- -- , atomicWriteCNFRef
       -- -- , atomicModifyCNFRef
       -- ) where

import Control.DeepSeq
import Data.CNFRef.DeepStrict
import Data.CNFRef.Internal
import Data.Compact.Indexed
import Data.IORef

newtype CNFRef s a = CNFRef (Compact s (IORef a))
-- RRN: I've had a change of heart, I think this should just be an alias:
-- type CNFRef s a = Compact s (IORef a)

newtype CIO s a = CIO (IO a) deriving (Functor, Applicative, Monad)

-- Dose Any help?  Alternative, actually copy a () into the Block.
type BlockChain s = Compact s Any

runCIO :: (forall s . CIO s a) -> IO a
runCIO (CIO fn) =
  do c <- newCompact 4096
     let CIO io = fn c
     io

ask :: CIO s (BlockChain s)
ask = undefined
  
newCNFRef' :: a -> CIO s (CNFRef s a)
newCNFRef' = undefined

newCompact' :: NFData a => a -> CIO s (Compact s a)
newCompact' = undefined

-- We expect: (1) DeepStrict a => NFData a
--            (2) DeepStrict a => rnf == seq

writeCNFRef' :: CNFRef s a -> Compact s a -> CIO s ()
writeCNFRef' = undefined

-- Factory methods:
----------------------------------------

factory :: CIO (blah -> IO blah)


--------------------------------------------------------------------------------

-- | Point a CNFRef at a new value that already lives in the correct
-- region.
writeCNFRef :: CNFRef s a -> Compact s a -> IO ()
writeCNFRef (CNFRef c) c' = do
  let ref = getCompact c
      a = getCompact c'
  writeIORef ref a

-- | Read the contents of the CNFRef, but don't lose track of the fact
--   that the value lives in the same compact region as the reference
--   that points to it.
readCNFRef :: DeepStrict a => CNFRef s a -> IO (Compact s a)
readCNFRef (CNFRef c) = do
  let ref = getCompact c
  a <- readIORef ref
  appendCompact c a


-- UNSAFE interface:
--------------------------------------------------------------------------------

-- | Copy a new value to the same compact region as the CNFRef.
--
--  (RRN) This could be obsoleted by a function of type (CNFRef s a ->
--  Compact s ()) but I'm not sure even that would be safe at the
--  moment.  Having a separate `Block` type is a better idea.
copyToCompact :: DeepStrict b => CNFRef s a -> b -> IO (Compact s b)
copyToCompact (CNFRef c) b = appendCompact c b  -- This will leak if b
                                                -- is not unboxed.

-- | Copy a boxed value into a compact region and create a new CNFRef
-- that points to it.
newCNFRef :: DeepStrict a => a -> IO (CNFRef s a)
newCNFRef a = do
  ref <- newIORef a
  let sz = unsafeSizeof ref
  c <- newCompact sz ref
  return $ CNFRef c

-- | Append a boxed value into an existing CNFRef and return a new
-- CNFRef that points to it.
appendCNFRef :: DeepStrict b => CNFRef s a -> b -> IO (CNFRef s b)
appendCNFRef (CNFRef c) b = do
  ref' <- newIORef b
  c' <- appendCompact c ref'
  return $ CNFRef c'


-- modifyCNFRef :: DeepStrict a => CNFRef s a -> (a -> Compact s a) -> IO ()
-- modifyCNFRef ref f = undefined

-- atomicWriteCNFRef :: CNFRef s a -> In s a -> IO ()
-- atomicWriteCNFRef (CNFRef c) _ = undefined

-- atomicModifyCNFRef :: DeepStrict a => CNFRef s a -> (In s a -> In s a) -> IO ()
-- atomicModifyCNFRef ref f = undefined
