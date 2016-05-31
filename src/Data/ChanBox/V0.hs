{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards           #-}

-- |

module Data.ChanBox.V0 where

import Control.DeepSeq
import Control.Monad               as M
import Data.IORef
import Data.Vector                 as B
import Data.Vector.Unboxed.Mutable as V

type Msg = IOVector Int

type Chan s = IORef (Vector Msg)

data ChanBox = forall s. ChanBox { box :: Chan s }

instance NFData ChanBox where
  rnf ChanBox { .. } = rnf box

newMessage :: ChanBox -> Int -> IO Msg
newMessage ChanBox { .. } n = V.replicate 1024 n

newBox :: IO ChanBox
newBox = do
  box <- newIORef empty
  return $ ChanBox box

lengthChan :: Chan s -> IO Int
lengthChan chan =
  B.length <$> readIORef chan

sizeBox :: ChanBox -> IO Int
sizeBox ChanBox { .. } = lengthChan box

maxLengthChan :: Int
maxLengthChan = 200000

dropMinChan :: ChanBox -> IO ()
dropMinChan ChanBox { .. } = do
  vec <- readIORef box
  let l = B.length vec
  when (l >= maxLengthChan) $ do
    let vec' = B.tail vec
    writeIORef box vec'

pushMsg :: ChanBox -> Msg -> IO ()
pushMsg b@ChanBox { .. } msg = do
  dropMinChan b
  vec <- readIORef box
  let vec' = snoc vec msg
  writeIORef box vec'
