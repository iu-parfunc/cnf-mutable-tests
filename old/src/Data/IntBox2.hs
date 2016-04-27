{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE Strict                    #-}
{-# LANGUAGE StrictData                #-}

-- | An example data structure using CNFRef

module Data.IntBox2
       -- (IntBox, newIntBox, readIntBox, writeIntBox)
where

import Control.Monad.Trans
import Data.CNFRef
import Data.Compact.Indexed
import Data.Traversable
import Data.Vector.Unboxed.Mutable as V

-- | An IntBox contains an existentially-bound private region:
data IntBox = forall s. IntBox { _box      :: CNFRef s Int
                               -- , _block :: BlockChain s
                               }

newIntBox :: IO IntBox
newIntBox = 
  runCIO $ do
    ref <- newCNFRef 0
    return $ IntBox ref 

readIntBox :: IntBox -> IO Int
readIntBox IntBox { .. } =
  do c <- readCNFRef _box     
     return (getCompact c)

-- Leaks memory!  Only for demonstration purposes:
writeIntBox :: IntBox -> Int -> IO ()
writeIntBox IntBox { .. } n =
  do c <- appendCompact _box n
     writeCNFRef _box c

