{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE Strict                    #-}
{-# LANGUAGE StrictData                #-}

-- | An example data structure using CNFRef

module Data.IntBox (IntBox, newIntBox, readIntBox, writeIntBox) where

import Control.Monad.Trans
import Data.CNFRef
import Data.Compact.Indexed
import Data.Traversable
import Data.Vector.Unboxed.Mutable as V

-- | An IntBox contains an existentially-bound private region:
data IntBox = forall s. IntBox { ref :: CNFRef s (IOVector Int) }

-- | Create an empty IntBox.
newIntBox :: IO IntBox
newIntBox = do
  vec <- unsafeNew 0
  runCIO $ do
    ref <- newCNFRef vec
    return $ IntBox ref

-- | Read all values from the IntBox.
readIntBox :: IntBox -> IO [Int]
readIntBox IntBox { .. } = do
  c <- readCNFRef ref
  let vec = getCompact c
      len = V.length vec
  forM [0 .. len - 1] $ unsafeRead vec

-- | Insert a value into the IntBox.
writeIntBox :: IntBox -> Int -> IO ()
writeIntBox IntBox { .. } n = do
  c <- readCNFRef ref
  let vec = getCompact c
      len = V.length vec
  vec' <- grow vec 1
  unsafeWrite vec' len n
  c' <- newCompactIn ref vec'
  writeCNFRef ref c'
