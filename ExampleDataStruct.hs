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
writeIntBox n (IntBox ref) =
  do let cr = unsafeGetCompact ref -- Just to get which Compact its in.
     n' <- appendCompact cr n -- FIXME!  Make this work on unboxed so we don't leak.
     writeCNFRef ref n'

readIntBox :: IntBox -> IO Int
readIntBox (IntBox r) =
  do c <- readCNFRef r
     return (getCompact c)



