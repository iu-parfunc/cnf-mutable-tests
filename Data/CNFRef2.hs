{-# LANGUAGE BangPatterns #-}

module Data.CNFRef2
       ( CNFRef(..) -- Transparent for now...
       , unsafeGetCompact
       , DeepStrict (..)
       , newCNFRef
       , readCNFRef
       , writeCNFRef
       -- , modifyCNFRef
       -- , atomicWriteCNFRef
       -- , atomicModifyCNFRef
       ) where

import Control.DeepSeq
import Data.Compact.Indexed
import Data.IORef
import System.IO.Unsafe

instance NFData a => NFData (IORef a) where
  rnf a = unsafePerformIO $ modifyIORef' a force

newtype CNFRef s a = CNFRef (Compact s (IORef a))


-- | A hypothetical typeclass to track transitive strictness.
--   I.e. datatypes whose values cannot contain (reach) a thunk.
class NFData a => DeepStrict a where

instance DeepStrict Int

-- | TODO: Replace this with a function that just extracts the "Block", not a Compact.
--   Or at the very least cast it so it returns `Compact s ()`
unsafeGetCompact :: CNFRef s a -> Compact s (IORef a)
unsafeGetCompact (CNFRef c) = c


-- | Copy a boxed value into a compact region and create a new CNFRef
-- that points to it.
newCNFRef :: DeepStrict a => a -> IO (CNFRef s a)
newCNFRef c = undefined

-- | Read the contents of the CNFRef, but don't lose track of the fact
--   that the value ilives in the same compact region as the reference
--   that points to it.
readCNFRef :: DeepStrict a => CNFRef s a -> IO (Compact s a)
readCNFRef (CNFRef c) = undefined

-- | Point a CNFRef at a new value that already lives in the correct
-- region.
writeCNFRef :: CNFRef s a -> Compact s a -> IO ()
writeCNFRef (CNFRef !c) x = undefined

-- modifyCNFRef :: DeepStrict a => CNFRef s a -> (a -> Compact s a) -> IO ()
-- modifyCNFRef !ref !f = undefined

-- atomicWriteCNFRef :: CNFRef s a -> In s a -> IO ()
-- atomicWriteCNFRef (CNFRef !c) _ = undefined

-- atomicModifyCNFRef :: DeepStrict a => CNFRef s a -> (In s a -> In s a) -> IO ()
-- atomicModifyCNFRef !ref !f = undefined
