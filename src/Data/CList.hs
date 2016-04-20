{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE Strict                    #-}
{-# LANGUAGE StrictData                #-}

-- | A linked-list like data structure in a compact

module Data.CList (
    CList,
    newCList,
    readCList,
    pushCList,
    popCList,
    sizeCList,
    ) where

import Control.Monad
import Control.DeepSeq
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

instance NFData (CList a) where
  rnf _c = ()

-- | Create a new CList
newCList :: DeepStrict a => IO (CList a)
newCList = do
  root <- newCNFRef Nil
  free <- appendCNFRef root Nil
  return $ CList root free

-- | Read out a CList
readCList :: (DeepStrict a, Unbox a) => CList a -> IO [a]
readCList CList { .. } = readMList rootList

-- | Write a new value at the end of the CList
pushCList :: (DeepStrict a, Unbox a, Eq a) => CList a -> a -> IO ()
pushCList CList { .. } a = do
  c <- dropMList freeList a
  case getCompact c of
    Nil -> do
      vec <- newVec a
      ref <- newIORef Nil
      c' <- copyToCompact rootList $ Cons vec ref
      appendMList rootList c'
    Cons _ _ ->
      appendMList rootList c

-- | Drop the value at the end of the CList
popCList :: (DeepStrict a, Unbox a, Eq a) => CList a -> IO (Maybe a)
popCList CList { .. } = do
  c <- popMList rootList
  let v = getCompact c
  case v of
    Just a -> do
      c' <- copyToCompact rootList a
      updateMList freeList c'
    Nothing -> return ()
  return v

-- | Return the total size of the CList (including free list)
sizeCList :: (DeepStrict a, Unbox a) => CList a -> IO Int
sizeCList CList { .. } =
  liftM2 (+) (lengthMList rootList) (lengthMList freeList)
