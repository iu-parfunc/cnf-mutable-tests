{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE Strict                    #-}
{-# LANGUAGE StrictData                #-}

-- |

module Data.ChanBox.V1 where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Utils
import Data.Atomics.Counter
import Data.CNFRef.DeepStrict
import Data.CNFRef.Internal
import Data.Compact.Indexed
import Data.Vector                 as V
import Data.Vector.Unboxed.Mutable as VU
import GHC.Generics

type Msg = IOVector Int

instance DeepStrict a => DeepStrict (Vector a)

instance NFData AtomicCounter where
  rnf _ = ()

instance DeepStrict AtomicCounter where

data Chan s = Chan { vec   :: Compact s (Vector Msg)
                   , front :: AtomicCounter
                   , rear  :: AtomicCounter
                   }
  deriving (Generic, NFData, DeepStrict)

data ChanBox = forall s. ChanBox { box :: Chan s }

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
  front <- newCounter 0
  rear <- newCounter 0
  vec <- V.replicateM (maxSize + 1) (VU.replicate 1024 0)
  c <- newCompactNoShare (unsafeSizeof vec) vec
  let box = Chan c front rear
  return $ ChanBox box

lengthChan :: Chan s -> IO Int
lengthChan Chan { .. } = do
  start <- readCounter front
  end <- readCounter rear
  let maxSize = V.length (getCompact vec)
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
      start <- readCounter front
      let maxSize = V.length (getCompact vec)
          start' = (start + 1) `mod` maxSize
      writeCounter front start'

pushMsg :: ChanBox -> Msg -> IO ()
pushMsg b@ChanBox { .. } msg =
  case box of
    Chan { .. } -> do
      size <- lengthChan box
      let maxSize = V.length (getCompact vec)
      when (size == maxSize - 1) $ dropMinChan b
      end <- readCounter rear
      let end' = (end + 1) `mod` maxSize
          v = getCompact vec
          msg' = v ! end
      for_ 0 1023 $ \i -> do
        a <- unsafeRead msg i
        c <- appendCompactNoShare vec a
        -- FIXME: unsafe interface
        unsafeWrite msg' i (getCompact c)
      writeCounter rear end'
