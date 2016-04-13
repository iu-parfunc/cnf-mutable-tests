{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE Strict                    #-}
{-# LANGUAGE StrictData                #-}

-- | CList variant with no freeList

module Data.CList.NoFree (
    CList,
    newCList,
    readCList,
    pushCList,
    popCList,
    sizeCList,
    ) where

import Data.CList.MList
import Data.CNFRef
import Data.CNFRef.DeepStrict
import Data.Compact.Indexed
import Data.IORef
import Data.Vector.Unboxed.Mutable

-- | A compact list
data CList a = forall s. CList { rootList :: MList s a -- ^ pointer to root list
                               }

-- | Create a new CList
newCList :: DeepStrict a => IO (CList a)
newCList = do
  root <- newCNFRef Nil
  return $ CList root

-- | Read out a CList
readCList :: (DeepStrict a, Unbox a) => CList a -> IO [a]
readCList CList { .. } = readMList rootList

-- | Write a new value at the end of the CList
pushCList :: (DeepStrict a, Unbox a, Eq a) => CList a -> a -> IO ()
pushCList CList { .. } a = do
  vec <- newVec a
  ref <- newIORef Nil
  c <- copyToCompact rootList $ Cons vec ref
  appendMList rootList c

-- | Drop the value at the end of the CList
popCList :: (DeepStrict a, Unbox a, Eq a) => CList a -> IO (Maybe a)
popCList CList { .. } = getCompact <$> popMList rootList

-- | Return the total size of the CList (including free list)
sizeCList :: (DeepStrict a, Unbox a) => CList a -> IO Int
sizeCList CList { .. } = lengthMList rootList
