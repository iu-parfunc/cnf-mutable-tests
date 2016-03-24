{-# LANGUAGE GADTs, ExistentialQuantification, ScopedTypeVariables #-}

-- | 

module ExampleDataStruct where

import Data.Compact.Indexed
import Data.CNFRef2

-- import Data.Vector.Unboxed

-- | An IntBox contains an existentially-bound private region:
data IntBox = forall s . IntBox (CNFRef s Int)
-- ^ FIXME: this should use an unboxed vector, and store an unboxed Int.
--   Storing Ints creates a silly memory leak problem.


newIntBox :: IO IntBox
newIntBox = undefined

writeIntBox :: Int -> IntBox -> IO ()
writeIntBox n (IntBox r) = undefined

readIntBox :: IntBox -> IO Int
readIntBox (IntBox r) = undefined



