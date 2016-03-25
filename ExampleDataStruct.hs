{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ScopedTypeVariables       #-}

-- | An example data structure using CNFRef

module ExampleDataStruct where

import Data.CNFRef2
import Data.Compact.Indexed

-- import Data.Vector.Unboxed

-- | An IntBox contains an existentially-bound private region:
data IntBox = forall s . IntBox (CNFRef s Int)
-- ^ FIXME: this should use an unboxed vector, and store an unboxed Int.
--   Storing Ints creates a silly memory leak problem.

newIntBox :: Int -> IO IntBox
newIntBox !n = IntBox <$> newCNFRef n

writeIntBox :: Int -> IntBox -> IO ()
writeIntBox !n (IntBox !ref) = do
  c <- copyToCompact ref n
  writeCNFRef ref c

readIntBox :: IntBox -> IO Int
readIntBox (IntBox !ref) = do
  !c <- readCNFRef ref
  return (getCompact c)
