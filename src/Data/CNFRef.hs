{-# LANGUAGE BangPatterns #-}

module Data.CNFRef
       ( CNFRef(..) -- Transparent for now...
       , copyToCompact
       , newCNFRef
       , readCNFRef
       , writeCNFRef
       -- , modifyCNFRef
       -- , atomicWriteCNFRef
       -- , atomicModifyCNFRef
       ) where

import Control.DeepSeq
import Data.CNFRef.DeepStrict
import Data.Compact.Indexed
import Data.IORef

newtype CNFRef s a = CNFRef (Compact s (IORef a))

-- | Copy a new value to the same compact region as the CNFRef.
copyToCompact :: DeepStrict a => CNFRef s a -> a -> IO (Compact s a)
copyToCompact (CNFRef !c) !a = appendCompact c a  -- This will leak if
                                                  -- a is not unboxed.

-- | Copy a boxed value into a compact region and create a new CNFRef
-- that points to it.
newCNFRef :: DeepStrict a => a -> IO (CNFRef s a)
newCNFRef !a = do
  !ref <- newIORef a
  -- what's the correct size?
  let sz = 4096
  !c <- newCompact sz ref
  return $! CNFRef c

-- | Read the contents of the CNFRef, but don't lose track of the fact
--   that the value lives in the same compact region as the reference
--   that points to it.
readCNFRef :: DeepStrict a => CNFRef s a -> IO (Compact s a)
readCNFRef (CNFRef !c) = do
  let !ref = getCompact c
  !a <- readIORef ref
  appendCompact c a

-- | Point a CNFRef at a new value that already lives in the correct
-- region.
writeCNFRef :: CNFRef s a -> Compact s a -> IO ()
writeCNFRef (CNFRef !c) !c' = do
  let !ref = getCompact c
      !a = getCompact c'
  writeIORef ref a

-- modifyCNFRef :: DeepStrict a => CNFRef s a -> (a -> Compact s a) -> IO ()
-- modifyCNFRef !ref !f = undefined

-- atomicWriteCNFRef :: CNFRef s a -> In s a -> IO ()
-- atomicWriteCNFRef (CNFRef !c) _ = undefined

-- atomicModifyCNFRef :: DeepStrict a => CNFRef s a -> (In s a -> In s a) -> IO ()
-- atomicModifyCNFRef !ref !f = undefined
