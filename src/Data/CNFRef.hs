{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE Strict     #-}
{-# LANGUAGE StrictData #-}

module Data.CNFRef (
    CNFRef,
    BlockChain,
    CIO,
    runCIO,
    getBlockChain,
    newCNFRef,
    newCNFRefIn,
    newCNFRefIn',
    newCompactIn,
    readCNFRef,
    writeCNFRef,
    ) where

import Control.DeepSeq
import Control.Monad.Reader
import Data.CNFRef.DeepStrict
import Data.Compact.Indexed
import Data.IORef

-- | Mutable reference in a compact region.
newtype CNFRef s a = CNFRef (Compact s (IORef a))

instance DeepStrict a => NFData (CNFRef s a) where
  rnf _ = ()

instance DeepStrict a => DeepStrict (CNFRef s a) where

-- | Pointer to the root of a compact region.
type BlockChain s = Compact s ()

-- | An IO action which uses a compact region.
type CIO s a = ReaderT (BlockChain s) IO a

-- | Run a CIO action to an IO action, by creating a new compact
-- region.
runCIO :: (forall s. CIO s a) -> IO a
runCIO cio = do
  c <- newCompact 4096 ()
  runReaderT cio c

-- | Copy a boxed value into a compact region and create a new CNFRef
-- that points to it.
newCNFRef :: DeepStrict a => a -> CIO s (CNFRef s a)
newCNFRef a = do
  block <- ask
  liftIO $ do
    ref <- newIORef a
    c <- appendCompact block ref
    return $ CNFRef c

-- | Copy a boxed value into an existing compact region and return a
-- new CNFRef that points to it.
newCNFRefIn :: DeepStrict a => BlockChain s -> a -> IO (CNFRef s a)
newCNFRefIn blk b = do
  ref <- newIORef b
  c <- appendCompact blk ref
  return $ CNFRef c

-- | Copy a boxed value into an existing compact region and return a
-- new CNFRef that points to it.
newCNFRefIn' :: DeepStrict b => CNFRef s a -> b -> IO (CNFRef s b)
newCNFRefIn' (CNFRef c) b = do
  ref <- newIORef b
  c <- appendCompact c ref
  return $ CNFRef c

-- | Copy a boxed value into an existing compact region and return a
-- new Compact that points to it.
newCompactIn :: DeepStrict b => CNFRef s a -> b -> IO (Compact s b)
newCompactIn (CNFRef c) a = appendCompact c a

-- | Return the blockchain for a region.
getBlockChain :: CIO s (BlockChain s)
getBlockChain = ask

-- | Read the contents of the CNFRef, but don't lose track of the fact
-- that the value lives in the same compact region as the reference
-- that points to it.
readCNFRef :: DeepStrict a => CNFRef s a -> IO (Compact s a)
readCNFRef (CNFRef c) = do
  let ref = getCompact c
  a <- readIORef ref
  appendCompact c a

-- | Point a CNFRef at a new value that already lives in the correct
-- region.
writeCNFRef :: CNFRef s a -> Compact s a -> IO ()
writeCNFRef (CNFRef c) c' = do
  let ref = getCompact c
      a' = getCompact c'
  writeIORef ref a'
