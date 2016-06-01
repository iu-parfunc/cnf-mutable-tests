{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

-- |

module Data.ChanBox.V0 where

import Control.DeepSeq
import Control.Monad               as M
import Data.IORef
import Data.Vector                 as B
import Data.Vector.Unboxed.Mutable as V
import GHC.Generics

type Msg = IOVector Int

data Chan = Chan { vec   :: Vector Msg
                 , front :: IORef Int
                 , rear  :: IORef Int
                 }
  deriving (Generic, NFData)

data ChanBox = ChanBox { box :: Chan }

instance NFData ChanBox where
  rnf ChanBox { .. } = rnf box

newMessage :: ChanBox -> Int -> IO Msg
newMessage _ = V.replicate 1024

newBox :: IO ChanBox
newBox = newBox' 2000

newBox' :: Int -> IO ChanBox
newBox' maxSize = do
  front <- newIORef 0
  rear <- newIORef 0
  msg <- V.replicate 1024 0
  let vec = B.replicate (maxSize + 1) msg
  let box = Chan vec front rear
  return $ ChanBox box

lengthChan :: Chan -> IO Int
lengthChan Chan { .. } = do
  start <- readIORef front
  end <- readIORef rear
  let maxSize = B.length vec
  return $ if end >= start
             then end - start
             else end - start + maxSize

sizeBox :: ChanBox -> IO Int
sizeBox ChanBox { .. } = lengthChan box

dropMinChan :: ChanBox -> IO ()
dropMinChan ChanBox { .. } =
  case box of
    Chan { .. } -> do
      start <- readIORef front
      let maxSize = B.length vec
          start' = (start + 1) `mod` maxSize
      writeIORef front start'

pushMsg :: ChanBox -> Msg -> IO ()
pushMsg b@ChanBox { .. } msg =
  case box of
    Chan { .. } -> do
      size <- lengthChan box
      let maxSize = B.length vec
      when (size == maxSize - 1) $ dropMinChan b
      end <- readIORef rear
      let end' = (end + 1) `mod` maxSize
          msg' = vec ! end
      M.forM_ [0 .. 1023] $ \i -> do
        a <- V.read msg i
        V.write msg' i a
      writeIORef rear end'
