{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE Strict                    #-}
{-# LANGUAGE StrictData                #-}

-- | An example data structure using CNFRef

module Data.IntBox
       -- (IntBox, newIntBox, readIntBox, writeIntBox)
where

import Control.Monad.Trans
import Data.CNFRef
import Data.Compact.Indexed
import Data.Traversable
import Data.Vector.Unboxed.Mutable as V

-- | An IntBox contains an existentially-bound private region:
data IntBox = forall s. IntBox { _box      :: CNFRef s (IOVector Int)
                               , _block :: BlockChain s
                               -- , _readBox  :: CNFRef s (IOVector Int) -> CIO s [Int]
                               -- , _writeBox :: CNFRef s (IOVector Int) -> Int -> CIO s ()
                               }

newIntBox :: IO IntBox
newIntBox = do
  vec <- unsafeNew 1
  runCIO $ do
    ref <- newCNFRef vec
    bl  <- getBlockChain
    return $ IntBox ref bl

readIntBox :: IntBox -> IO Int
readIntBox IntBox { _box, _block } =
  do c <- readCNFRef _box     
     unsafeRead (getCompact c) 0

writeIntBox :: IntBox -> Int -> IO ()
writeIntBox IntBox { _box, _block } n =
  do -- c <- appendCompact _block n
     v <- readCNFRef _box
     unsafeWrite (getCompact v) 0 n
     -- writeCNFRef _box c

{-
readIntBox :: CNFRef s (IOVector Int) -> CIO s [Int]
readIntBox ref = runCIO $ do c <- readCNFRef _box
                             let vec = getCompact c
                                 len = V.length vec
                             liftIO $ forM [0 .. len - 1] $ unsafeRead vec
  
readIntBox :: IntBox -> IO [Int]
readIntBox IntBox { .. } = undefined -- runCIO $ _readBox _box

writeIntBox :: IntBox -> IO [Int]
writeIntBox IntBox { .. } = undefined -- runCIO $ _writeBox _box

mkCNFReader :: CNFRef s (IOVector Int) -> CIO s (IO [Int])
mkCNFReader ref = undefined
  
{-
readBox :: CIO s (CNFRef s (IOVector Int) -> IO [Int])
readBox = return $ \ref -> do
  c <- readCNFRef ref
  let vec = getCompact c
      len = V.length vec
  liftIO . forM [0 .. len - 1] $ unsafeRead vec
-}

writeBox :: forall s. CNFRef s (IOVector Int) -> Int -> CIO s ()
writeBox ref n = do
  c <- readCNFRef ref
  let vec = getCompact c
      len = V.length vec
  vec' <- liftIO $ do
            vec' <- grow vec 1
            unsafeWrite vec' len n
            return vec'
  c' <- copyToCompact vec'
  writeCNFRef ref c'
-}
