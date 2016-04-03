{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE StrictData                #-}

-- | A linked-list like data structure in a compact

module Data.CList (
    CList,
    newCList,
    readCList,
    writeCList,
    popCList,
    sizeCList,
    ) where

import Control.Monad
import Data.CList.MList
import Data.CNFRef
import Data.CNFRef.DeepStrict
import Data.Compact.Indexed
import Data.IORef
import Data.Vector.Unboxed.Mutable

-- | A compact list
data CList a = forall s. CList { rootList :: MList s a -- ^ pointer to root list
                               , freeList :: MList s a -- ^ pointer to free list
                               }

-- | Create a new CList
newCList :: DeepStrict a => IO (CList a)
newCList = do
  root <- newCNFRef Nil
  free <- newCNFRef Nil
  -- FIXME:
  -- free <- appendCNFRef root Nil
  return $ CList root free

-- | Read out a CList
readCList :: (DeepStrict a, Unbox a) => CList a -> IO [a]
readCList CList { .. } = readMList rootList

-- | Write a new value at the end of the CList
writeCList :: (DeepStrict a, Unbox a, Eq a) => CList a -> a -> IO ()
writeCList CList { .. } a = do
  c <- dropMList freeList a
  case getCompact c of
    Nil -> do
      vec <- newVec a
      ref <- newIORef Nil
      c' <- copyToCompact rootList $ Cons vec ref
      writeMList rootList c'
    Cons _ _ ->
      writeMList rootList c

-- | Drop the value at the end of the CList
popCList :: (DeepStrict a, Unbox a, Eq a) => CList a -> IO (Maybe a)
popCList CList { .. } = do
  v <- popMList rootList
  case v of
    Just a  -> updateMList freeList a
    Nothing -> return ()
  return v

-- | Return the total size of the CList (including free list)
sizeCList :: (DeepStrict a, Unbox a) => CList a -> IO Int
sizeCList CList { .. } =
  liftM2 (+) (lengthMList rootList) (lengthMList freeList)
