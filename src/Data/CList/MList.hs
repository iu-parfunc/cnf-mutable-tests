{-# LANGUAGE DeriveAnyClass     #-}
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
data List s a where
  Nil :: List s a
  Cons :: IOVector a          -- ^ car is an unboxed mutable vector
       -> CNFRef s (List s a) -- ^ cdr is a mutable pointer
       -> List s a

deriving instance Generic (List s a)
deriving instance NFData (List s a)
deriving instance DeepStrict (List s a)

-- | A cons-list in a mutable compact region
type MList s a = CNFRef s (List s a)

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
      c' <- readCNFRef ref
      l <- go $ getCompact c'
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
      p <- readCNFRef ref
      as <- go $ getCompact p
      return $ a : as

-- | Insert a value into a MList if it doesn't already exist
updateMList :: (DeepStrict a, Unbox a, Eq a) => MList s a -> Compact s a -> IO ()
updateMList m ca = do
  let a = getCompact ca
  c <- readCNFRef m
  case getCompact c of
    Nil -> do
      vec <- newVec a
      cref <- newCNFRefIn' m Nil
      c' <- newCompactIn m $ Cons vec cref
      writeCNFRef m c'
    Cons vec ref -> do
      v <- readVec vec
      unless (v == a) $ go ref

  where
    go prev = do
      let a = getCompact ca
      cur <- readCNFRef prev
      case getCompact cur of
        Nil -> do
          vec <- newVec a
          ref <- newCNFRefIn' m Nil
          c <- newCompactIn m $ Cons vec ref
          writeCNFRef prev c
        Cons vec next -> do
          v <- readVec vec
          unless (v == a) $ go next

-- | Find a value inside an MList
findMList :: (DeepStrict a, Unbox a, Eq a) => MList s a -> a -> IO (Compact s (List s a))
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
              c' <- readCNFRef ref
              go c'

-- | Append a List at the end of an MList
appendMList :: DeepStrict a => MList s a -> Compact s (List s a) -> IO ()
appendMList m l = do
  c <- readCNFRef m
  case getCompact c of
    Nil        -> writeCNFRef m l
    Cons _ ref -> go ref

  where
    go prev = do
      cur <- readCNFRef prev
      case getCompact cur of
        Nil         -> writeCNFRef prev l
        Cons _ next -> go next

-- | Drop a value from an MList
dropMList :: (DeepStrict a, Unbox a, Eq a) => MList s a -> a -> IO (Compact s (List s a))
dropMList m a = do
  c <- readCNFRef m
  case getCompact c of
    Nil -> return c
    Cons vec ref -> do
      v <- readVec vec
      p <- readCNFRef ref
      case getCompact p of
        Nil -> do
          c' <- newCompactIn m Nil
          if v == a
            then do
              writeCNFRef m c'
              return c
            else return c'
        Cons _ ref' ->
          if v == a
            then do
              p' <- readCNFRef ref'
              writeCNFRef m p'
              return c
            else do
              ref'' <- go ref ref'
              readCNFRef ref''

  where
    go pprev prev = do
      cur <- readCNFRef prev
      case getCompact cur of
        Nil -> do
          Cons vec _ <- getCompact <$> readCNFRef pprev
          v <- readVec vec
          if v == a
            then do
              writeCNFRef pprev cur
              return prev
            else newCNFRefIn' m Nil
        Cons _ ref -> do
          Cons vec _ <- getCompact <$> readCNFRef pprev
          v <- readVec vec
          if v == a
            then do
              writeCNFRef pprev cur
              return prev
            else go prev ref

-- | Drop the last value in the MList
popMList :: (DeepStrict a, Unbox a) => MList s a -> IO (Compact s (Maybe a))
popMList m = do
  c <- readCNFRef m
  case getCompact c of
    Nil -> newCompactIn m Nothing
    Cons vec ref -> do
      p <- readCNFRef ref
      case getCompact p of
        Nil -> do
          c' <- newCompactIn m Nil
          writeCNFRef m c'
          a <- readVec vec
          newCompactIn m $ Just a
        Cons _ ref' -> go ref ref'

  where
    go pprev prev = do
      cur <- readCNFRef prev
      case getCompact cur of
        Nil -> do
          Cons vec _ <- getCompact <$> readCNFRef pprev
          a <- readVec vec
          c <- newCompactIn m Nil
          writeCNFRef pprev c
          newCompactIn m $ Just a
        Cons _ ref -> go prev ref
