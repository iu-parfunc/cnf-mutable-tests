{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Strict             #-}
{-# LANGUAGE StrictData         #-}

-- | A cons-list like data structure in a mutable compact region

module Data.CList.MList where

import Control.DeepSeq
import Control.Monad
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

-- | A cons-list in a mutable compact region
type MList s a = CNFRef s (List a)

-- | Write a new value into an IORef, after strictly evaluating it.
writeIORef' :: NFData a => IORef a -> a -> IO ()
writeIORef' ref a = a `deepseq` writeIORef ref a

-- | Create new singleton vector
newVec :: Unbox a => a -> IO (IOVector a)
newVec a = do
  vec <- unsafeNew 1
  unsafeWrite vec 0 a
  return vec

-- | Read the singleton in the vector
readVec :: Unbox a => IOVector a -> IO a
readVec vec = unsafeRead vec 0

-- | Update the singleton value in the vector
writeVec :: Unbox a => IOVector a -> a -> IO ()
writeVec vec = unsafeWrite vec 0

-- | Return length of the MList
lengthMList :: (DeepStrict a, Unbox a) => MList s a -> IO Int
lengthMList m = do
  c <- readCNFRef m
  go $ getCompact c

  where
    go Nil = return 0
    go (Cons vec ref) = do
      as <- readIORef ref
      l <- go as
      return $ V.length vec + l

-- | Read out all values in the MList
readMList :: (DeepStrict a, Unbox a) => MList s a -> IO [a]
readMList l = do
  c <- readCNFRef l
  go $ getCompact c

  where
    go Nil = return []
    go (Cons vec ref) = do
      a <- readVec vec
      p <- readIORef ref
      as <- go p
      return $ a : as

-- | Insert a value into a MList if it doesn't already exist
updateMList :: (DeepStrict a, Unbox a, Eq a) => MList s a -> Compact s a -> IO ()
updateMList m ca = do
  let a = getCompact ca
  c <- readCNFRef m
  case getCompact c of
    Nil -> do
      vec <- newVec a
      ref <- newIORef Nil
      c' <- copyToCompact m $ Cons vec ref
      writeCNFRef m c'
    Cons vec ref -> do
      v <- readVec vec
      unless (v == a) $ go ref

  where
    go prev = do
      let a = getCompact ca
      cur <- readIORef prev
      case cur of
        Nil -> do
          vec <- newVec a
          ref <- newIORef Nil
          c <- copyToCompact m $ Cons vec ref
          writeIORef' prev $ getCompact c
        Cons vec next -> do
          v <- readVec vec
          unless (v == a) $ go next

-- | Find a value inside an MList
findMList :: (DeepStrict a, Unbox a, Eq a) => MList s a -> a -> IO (Compact s (List a))
findMList l a = readCNFRef l >>= go
  where
    go c =
      case getCompact c of
        Nil -> return c
        Cons vec ref -> do
          v <- readVec vec
          if v == a
            then return c
            else do
              p <- readIORef ref
              c' <- appendCompact c p
              go c'

-- | Append a List at the end of an MList
appendMList :: DeepStrict a => MList s a -> Compact s (List a) -> IO ()
appendMList m l = do
  c <- readCNFRef m
  case getCompact c of
    Nil        -> writeCNFRef m l
    Cons _ ref -> go ref

  where
    go prev = do
      cur <- readIORef prev
      case cur of
        Nil         -> writeIORef' prev $ getCompact l
        Cons _ next -> go next

-- | Drop a value from an MList
dropMList :: (DeepStrict a, Unbox a, Eq a) => MList s a -> a -> IO (Compact s (List a))
dropMList m a = do
  c <- readCNFRef m
  case getCompact c of
    Nil -> return c
    Cons vec ref -> do
      v <- readVec vec
      p <- readIORef ref
      case p of
        Nil -> do
          c' <- copyToCompact m Nil
          if v == a
            then do
              writeCNFRef m c'
              return c
            else return c'
        Cons _ ref' ->
          if v == a
            then do
              p' <- readIORef ref'
              c' <- copyToCompact m p'
              writeCNFRef m c'
              return c
            else do
              ref'' <- go ref ref'
              p' <- readIORef ref''
              copyToCompact m p'

  where
    go pprev prev = do
      cur <- readIORef prev
      case cur of
        Nil -> do
          Cons vec _ <- readIORef pprev
          v <- readVec vec
          if v == a
            then do
              writeIORef' pprev cur
              return prev
            else newIORef Nil
        Cons _ ref -> do
          Cons vec _ <- readIORef pprev
          v <- readVec vec
          if v == a
            then do
              writeIORef' pprev cur
              return prev
            else go prev ref

-- | Drop the last value in the MList
popMList :: (DeepStrict a, Unbox a) => MList s a -> IO (Compact s (Maybe a))
popMList m = do
  c <- readCNFRef m
  case getCompact c of
    Nil -> copyToCompact m Nothing
    Cons vec ref -> do
      p <- readIORef ref
      case p of
        Nil -> do
          c' <- copyToCompact m Nil
          writeCNFRef m c'
          a <- readVec vec
          copyToCompact m $ Just a
        Cons _ ref' -> go ref ref'

  where
    go pprev prev = do
      cur <- readIORef prev
      case cur of
        Nil -> do
          Cons vec _ <- readIORef pprev
          a <- readVec vec
          c <- copyToCompact m Nil
          writeIORef' pprev $ getCompact c
          copyToCompact m $ Just a
        Cons _ ref -> go prev ref
