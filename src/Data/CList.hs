{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE StrictData                #-}

-- | A linked-list like data structure in a compact

module Data.CList where

import Control.DeepSeq
import Data.CNFRef
import Data.CNFRef.DeepStrict
import Data.Compact.Indexed
import Data.IORef
import Data.Vector.Unboxed.Mutable as V
import GHC.Generics

-- | A cons-list with mutable cells and pointers
data List a where
  Nil :: List a
  Cons :: IOVector a     -- ^ car is an unboxed mutable vector
       -> IORef (List a) -- ^ cdr is a mutable pointer
       -> List a

deriving instance Generic (List a)

instance NFData a => NFData (List a)
instance DeepStrict a => DeepStrict (List a)

-- | A compact list
data CList a = forall s. CList { rootList :: CNFRef s (List a) -- ^ root
                               , freeList :: CNFRef s (List a) -- ^ free
                               }

newCList :: DeepStrict a => IO (CList a)
newCList = do
  r <- newCNFRef Nil
  f <- newCNFRef Nil
  return $ CList r f

readCList :: (DeepStrict a, Unbox a) => CList a -> IO [a]
readCList CList { rootList } = do
  c <- readCNFRef rootList
  go (getCompact c)

  where
    go Nil = return []
    go (Cons vec ref) = do
      a <- unsafeRead vec 0
      l <- readIORef ref
      as <- go l
      return (a : as)

writeCList :: (DeepStrict a, Unbox a) => CList a -> a -> IO ()
writeCList CList { rootList } a = do
  c <- readCNFRef rootList
  vec' <- unsafeNew 1
  unsafeWrite vec' 0 a
  ref' <- newIORef Nil
  let l = Cons vec' ref'
  case getCompact c of
    Nil -> do
      c' <- copyToCompact rootList l
      writeCNFRef rootList c'
    Cons vec ref -> do
      p <- readIORef ref
      go ref p l

  where
    go prev Nil l = writeIORef prev l
    go prev (Cons vec ref) l = do
      p <- readIORef ref
      go ref p l

popCList :: DeepStrict a => CList a -> IO ()
popCList CList { rootList } = do
  c <- readCNFRef rootList
  case getCompact c of
    Nil -> return ()
    Cons vec ref -> do
      p <- readIORef ref
      case p of
        Nil -> do
          c' <- copyToCompact rootList Nil
          writeCNFRef rootList c'
        Cons vec' ref' -> do
          p' <- readIORef ref'
          go ref ref' p'

  where
    go pprev prev Nil = writeIORef pprev Nil
    go pprev prev (Cons vec ref) = do
      p <- readIORef ref
      go prev ref p
