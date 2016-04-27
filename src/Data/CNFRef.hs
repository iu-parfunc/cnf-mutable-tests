{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict     #-}
{-# LANGUAGE StrictData #-}

module Data.CNFRef 
       ( CNFRef     -- Transparent for now...
       , BlockChain -- TODO: Should be defined elsewhere.
       , CIO, runCIO, getBlockChain
       , copyToCompact
       , newCNFRef
       , newCNFRefIn
       , readCNFRef
       , writeCNFRef
       -- , modifyCNFRef
       -- , atomicWriteCNFRef
       -- , atomicModifyCNFRef
       ) where

import Control.DeepSeq
import Control.Monad.Reader
import Data.CNFRef.DeepStrict
-- import Data.CNFRef.Internal
import Data.Compact.Indexed
import Data.IORef

-- It looks like this is GONE in ghc-prim-0.5.0.0
import GHC.Base (Any)


-- TODO: Sort out the DeepStrict/NFData story
--------------------------------------------------------------------------------
-- We expect: (1) DeepStrict a => NFData a
--            (2) DeepStrict a => rnf == seq

-- Converseley, we can have a newtype wrapper around anything in NFData
-- to make it DeepStrict, provided that we rnf it in the conversion.

--------------------------------------------------------------------------------

type CNFRef s a = Compact s (IORef a)

-- type BlockChain s = Compact s Any
-- Dose `Any` help?  Alternative, we can actually copy a () into the Block.
-- Actually, we *must* pass something to newCompact currently.
-- It doesn't let us allocate a truly EMPTY block chain.
type BlockChain s = Compact s ()

type CIO s a = ReaderT (BlockChain s) IO a

runCIO :: (forall s. CIO s a) -> IO a
runCIO cio = do
  c <- newCompact 4096 ()
  runReaderT cio c

newCNFRef :: DeepStrict a => a -> CIO s (CNFRef s a)
newCNFRef a = do
  block <- ask
  liftIO $ do
    ref <- newIORef a
    appendCompact block ref

-- | Append a boxed value into an existing CNFRef and return a new
-- CNFRef that points to it.
newCNFRefIn :: NFData b => BlockChain s -> b -> IO (CNFRef s b)
newCNFRefIn blk b = do
  ref' <- newIORef b
  c'   <- appendCompact blk ref' -- This had better copy thru the ref...
  return c'



getBlockChain :: CIO s (BlockChain s)
getBlockChain = ask

copyToCompact :: DeepStrict a => a -> CIO s (Compact s a)
copyToCompact a = do
  block <- ask
  liftIO $ appendCompact block a

-- TODO: switch to IO
readCNFRef :: DeepStrict a => CNFRef s a -> IO (Compact s a)
readCNFRef c = do
  let ref = getCompact c
  a <- readIORef ref
  appendCompact c a

-- TODO: switch to IO
writeCNFRef :: CNFRef s a -> Compact s a -> IO ()
writeCNFRef c c' = do
  let ref = getCompact c
      a' = getCompact c'
  writeIORef ref a'

-- -- Factory methods:
-- ----------------------------------------

mkReadCNFRef :: DeepStrict a => CNFRef s a -> CIO s (IO a)
mkReadCNFRef = error "mkReadCNFRef Implement me, I'm part of the trusted abstraction."

-- readFactory :: CIO s (CNFRef s a -> IO a)
-- readFactory = undefined

-- -- Separate constructor to avoid impredicativity polymporphism problems.
-- newtype Appender s = Appender (forall a . NFData a => a -> IO (Compact s a))

-- -- appendFactory :: forall s . CIO s (forall a . NFData a => a -> IO (Compact s a))
-- appendFactory :: forall s . CIO s (Appender s)
-- appendFactory = undefined

-- -- Example existential package:
-- ----------------------------------------

-- data MyRef a =
--   forall s .
--   MyRef { ref       :: CNFRef s a
--         , readMyRef :: IO a -- This one is not really necessary, see readCNFRef.
--         , append :: (forall x . NFData x => x -> IO (Compact s x))
--         }

-- POSSIBLE PERF PROBLEM:
----------------
-- Methods like writeMyRef could work well with an inlined MyRef record.
-- However, the code for newMyRef is not going to get optimized.  The data
-- constructor application for `MyRef` happens at the end of several actiosn
-- nested inside the RTS of several binds.  Functions like appendFactory can
-- inline, but the newCNFRef (and underlying newIORef )will not, and will serve
-- as a barrier to optimization.
--
--
-- newMyRef :: NFData a => a -> IO (MyRef a)
-- newMyRef x = runCIO $ do
--   r  <- newCNFRef' x
--   rd <- readFactory
--   Appender ap <- appendFactory
--   return $ MyRef { ref       = r
--                  , readMyRef = rd r
--                  , append    = ap
--                  }

-- -- To do a write of this type requires
-- writeMyRef :: NFData a => MyRef a -> a -> IO ()
-- writeMyRef (MyRef {ref,append}) a  =
--   do a' <- append a
--      writeCNFRef ref a'

-- -- Alternative version:
-- readMyRef2 :: NFData a => MyRef a -> IO a
-- readMyRef2 MyRef {ref} =
--   do c <- readCNFRef ref
--      return $! getCompact c

-- -- These look like they ARE safe:
-- --------------------------------------------------------------------------------

-- -- -- | Point a CNFRef at a new value that already lives in the correct
-- -- -- region.
-- -- writeCNFRef :: CNFRef s a -> Compact s a -> IO ()
-- -- writeCNFRef (CNFRef c) c' = do
-- --   let ref = getCompact c
-- --       a = getCompact c'
-- --   writeIORef ref a

-- -- -- | Read the contents of the CNFRef, but don't lose track of the fact
-- -- --   that the value lives in the same compact region as the reference
-- -- --   that points to it.
-- -- readCNFRef :: NFData a => CNFRef s a -> IO (Compact s a)
-- -- readCNFRef (CNFRef c) = do
-- --   let ref = getCompact c
-- --   a <- readIORef ref
-- --   appendCompact c a

-- -- -- | Copy a new value to the same compact region as the CNFRef.
-- -- --
-- -- --  (RRN) This could be obsoleted by a function of type (CNFRef s a ->
-- -- --  Compact s ()) but I'm not sure even that would be safe at the
-- -- --  moment.  Having a separate `Block` type is a better idea.
-- -- copyToCompact :: NFData b => CNFRef s a -> b -> IO (Compact s b)
-- -- copyToCompact (CNFRef c) b = appendCompact c b  -- This will leak if b
-- --                                                 -- is not unboxed.


-- -- These are certainly UNSAFE:
-- --------------------------------------------------------------------------------

-- -- | Copy a boxed value into a compact region and create a new CNFRef
-- -- that points to it.
-- newCNFRef :: NFData a => a -> IO (CNFRef s a)
-- newCNFRef a = do
--   ref <- newIORef a
--   let sz = unsafeSizeof ref
--   c <- newCompact sz ref
--   return $ CNFRef c


-- -- modifyCNFRef :: NFData a => CNFRef s a -> (a -> Compact s a) -> IO ()
-- -- modifyCNFRef ref f = undefined

-- -- atomicWriteCNFRef :: CNFRef s a -> In s a -> IO ()
-- -- atomicWriteCNFRef (CNFRef c) _ = undefined

-- -- atomicModifyCNFRef :: NFData a => CNFRef s a -> (In s a -> In s a) -> IO ()
-- -- atomicModifyCNFRef ref f = undefined



-- -- ================================================================================
-- -- POSSIBLE PERF SOLUTION
-- -- Store the region directly, or a token corresponding to it
-- -- ================================================================================

-- -- Could use explicit tokens, but it's easier to use the `Compact s ()` itself:

-- -- -- This must be a hidden constructor
-- -- data Token s = Token
-- -- -- The only safe way to get a token:
-- -- getToken :: CIO s (Token s)
-- -- getToken = return Token
-- -- appendWToken :: Token s -> a -> IO (Compact s a)
-- -- appendWToken = undefined

-- data MyRef2 a =
--   forall s .
--   MyRef2 { box    :: CNFRef s a
--          , block  :: Compact s ()
--          }

-- writeMyRef2 :: NFData a => MyRef2 a -> a -> IO ()
-- writeMyRef2 (MyRef2 {box,block}) a =
--   do c <- appendCompact block a
--      writeCNFRef box c
