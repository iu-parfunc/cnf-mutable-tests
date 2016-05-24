{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE Strict                    #-}
{-# LANGUAGE StrictData                #-}

-- |

module Data.ChanBox.V1 where

import Control.DeepSeq
import Control.Monad               as M
import Data.CNFRef
import Data.CNFRef.DeepStrict
import Data.Compact.Indexed
import Data.Vector                 as B
import Data.Vector.Unboxed.Mutable as V
import GHC.Generics

type Msg = IOVector Int

type Chan s = CNFRef s (Vector Msg)

instance DeepStrict (Vector Msg) where

data ChanBox = forall s. ChanBox { box :: Chan s, free :: Chan s }

newMessage :: ChanBox -> Int -> IO Msg
newMessage ChanBox { .. } n =
  getCompact <$> newMessage' free n

newMessage' :: Chan s -> Int -> IO (Compact s Msg)
newMessage' free n = do
  c <- readCNFRef free
  let vec = getCompact c
  case B.length vec of
    0 -> do
      msg <- V.replicate 1024 n
      appendCompact c msg
    _ -> do
      let msg = B.head vec
      M.forM_ [0 .. 1023] $ V.write msg n
      c' <- appendCompact c $ B.tail vec
      writeCNFRef free c'
      appendCompact c msg

newBox :: IO ChanBox
newBox = runCIO $ do
  box <- newCNFRef empty
  free <- newCNFRef empty
  return $ ChanBox box free

lengthChan :: Chan s -> IO Int
lengthChan chan =
  B.length . getCompact <$> readCNFRef chan

sizeBox :: ChanBox -> IO Int
sizeBox ChanBox { .. } =
  liftM2 (+) (lengthChan box) (lengthChan free)

maxLengthChan :: Int
maxLengthChan = 200000

dropMinChan :: ChanBox -> IO ()
dropMinChan ChanBox { .. } = do
  c <- readCNFRef box
  c' <- readCNFRef free
  let boxVec = getCompact c
      freeVec = getCompact c'
      l = B.length boxVec
  when (l > maxLengthChan) $ do
    let v = B.head boxVec
        boxVec' = B.tail boxVec
        freeVec' = cons v freeVec
    c' <- appendCompact c freeVec'
    writeCNFRef free c'
    c' <- appendCompact c boxVec'
    writeCNFRef box c'

pushMsg :: ChanBox -> Msg -> IO ()
pushMsg b@ChanBox { .. } msg = do
  dropMinChan b
  c <- readCNFRef box
  let boxVec = getCompact c
      boxVec' = snoc boxVec msg
  c' <- appendCompact c boxVec'
  writeCNFRef box c'
