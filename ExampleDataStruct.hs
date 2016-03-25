{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ScopedTypeVariables       #-}

-- | An example data structure using CNFRef

module ExampleDataStruct where

import Data.CNFRef2
import Data.Compact.Indexed

import Data.Vector.Unboxed

-- | An IntBox contains an existentially-bound private region:
data IntBox = forall s.  IntBox (CNFRef s (Vector Int))

-- | Create an empty IntBox.
newIntBox :: IO IntBox
newIntBox = IntBox <$> newCNFRef empty

-- | Insert a value into the IntBox.
writeIntBox :: IntBox -> Int -> IO ()
writeIntBox (IntBox !ref) !n = do
  !c <- readCNFRef ref
  let !vec = getCompact c
      !vec' = snoc vec n
  !c <- copyToCompact ref vec'
  writeCNFRef ref c

-- | Read all values from the IntBox.
readIntBox :: IntBox -> IO [Int]
readIntBox (IntBox !ref) = do
  !c <- readCNFRef ref
  let !vec = getCompact c
  return (toList vec)
