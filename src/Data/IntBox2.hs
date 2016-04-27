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
                               , _block :: BlockChain s
                               -- , _readBox  :: CNFRef s (IOVector Int) -> CIO s [Int]
                               -- , _writeBox :: CNFRef s (IOVector Int) -> Int -> CIO s ()
                               }

newIntBox :: IO IntBox
newIntBox = 
  runCIO $ do
    ref <- newCNFRef 0
    bl  <- getBlockChain
    return $ IntBox ref bl

readIntBox :: IntBox -> IO Int
readIntBox IntBox { _box, _block } =
  do c <- readCNFRef _box     
     return (getCompact c)

-- Leaks memory!  Only for demonstration purposes:
writeIntBox :: IntBox -> Int -> IO ()
writeIntBox IntBox { _box, _block } n =
  do c <- appendCompact _block n
     writeCNFRef _box c

