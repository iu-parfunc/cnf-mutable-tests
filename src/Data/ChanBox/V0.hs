{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

-- |

module Data.ChanBox.V0 where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Utils
import Data.IORef
import Data.Vector                 as V
import Data.Vector.Unboxed.Mutable as VU
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

{-# INLINE newMessage #-}
newMessage :: ChanBox -> Int -> IO Msg
newMessage _ = VU.replicate 1024

{-# INLINE newBox' #-}
newBox' :: IO ChanBox
newBox' = newBox 2000

newBox :: Int -> IO ChanBox
newBox maxSize = do
  front <- newIORef 0
  rear <- newIORef 0
  vec <- V.replicateM (maxSize + 1) (VU.replicate 1024 0)
  let box = Chan vec front rear
  return $ ChanBox box

lengthChan :: Chan -> IO Int
lengthChan Chan { .. } = do
  start <- readIORef front
  end <- readIORef rear
  let maxSize = V.length vec
  return $ if end >= start
             then end - start
             else end - start + maxSize

{-# INLINE sizeBox #-}
sizeBox :: ChanBox -> IO Int
sizeBox ChanBox { .. } = lengthChan box

{-# INLINE dropMinChan #-}
dropMinChan :: ChanBox -> IO ()
dropMinChan ChanBox { .. } =
  case box of
    Chan { .. } -> do
      start <- readIORef front
      let maxSize = V.length vec
          start' = (start + 1) `mod` maxSize
      writeIORef front start'

pushMsg :: ChanBox -> Msg -> IO ()
pushMsg b@ChanBox { .. } msg =
  case box of
    Chan { .. } -> do
      size <- lengthChan box
      let maxSize = V.length vec
      when (size == maxSize - 1) $ dropMinChan b
      end <- readIORef rear
      let end' = (end + 1) `mod` maxSize
          msg' = vec ! end
      for_ 0 1023 $ \i -> do
        a <- unsafeRead msg i
        unsafeWrite msg' i a
      writeIORef rear end'
