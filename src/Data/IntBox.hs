{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ScopedTypeVariables       #-}

-- | An example data structure using CNFRef

module Data.IntBox where

import Data.CNFRef
import Data.Compact.Indexed
import Data.Traversable
import Data.Vector.Unboxed.Mutable as V

-- | An IntBox contains an existentially-bound private region:
data IntBox = forall s.  IntBox (CNFRef s (IOVector Int))

-- | Create an empty IntBox.
newIntBox :: IO IntBox
newIntBox = do
  !vec <- unsafeNew 0
  !c <- newCNFRef vec
  return $ IntBox c

-- | Insert a value into the IntBox.
writeIntBox :: IntBox -> Int -> IO ()
writeIntBox (IntBox !ref) !n = do
  !c <- readCNFRef ref
  let !vec = getCompact c
      !len = V.length vec
  !vec' <- grow vec 1
  unsafeWrite vec' len n
  !c <- copyToCompact ref vec'
  writeCNFRef ref c

-- | Read all values from the IntBox.
readIntBox :: IntBox -> IO [Int]
readIntBox (IntBox !ref) = do
  !c <- readCNFRef ref
  let !vec = getCompact c
      !len = V.length vec
  forM [0 .. len - 1] $ unsafeRead vec
