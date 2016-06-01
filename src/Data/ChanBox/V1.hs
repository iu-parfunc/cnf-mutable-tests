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
import Control.Monad               as M
import Control.Monad.Trans
import Data.CNFRef
import Data.CNFRef.DeepStrict
import Data.Compact.Indexed
import Data.Vector                 as B
import Data.Vector.Unboxed.Mutable as V
import GHC.Generics

type Msg = IOVector Int

instance DeepStrict a => DeepStrict (Vector a)

data Chan s = Chan { vec   :: Compact s (Vector Msg)
                   , front :: CNFRef s Int
                   , rear  :: CNFRef s Int
                   }
  deriving (Generic, NFData, DeepStrict)

data ChanBox = forall s. ChanBox { box :: Chan s }

instance NFData ChanBox where
  rnf ChanBox { .. } = rnf box

newMessage :: ChanBox -> Int -> IO Msg
newMessage _ = V.replicate 1024

newBox :: IO ChanBox
newBox = newBox' 2000

newBox' :: Int -> IO ChanBox
newBox' maxSize = runCIO $ do
  front <- newCNFRef 0
  rear <- newCNFRef 0
  msg <- V.replicate 1024 0
  let vec = B.replicate (maxSize + 1) msg
  c <- liftIO $ newCompactIn front vec
  let box = Chan c front rear
  return $ ChanBox box

lengthChan :: Chan s -> IO Int
lengthChan Chan { .. } = do
  start <- getCompact <$> readCNFRef front
  end <- getCompact <$> readCNFRef rear
  let maxSize = B.length (getCompact vec)
  return $ if end >= start
             then end - start
             else end - start + maxSize

sizeBox :: ChanBox -> IO Int
sizeBox ChanBox { .. } = lengthChan box

dropMinChan :: ChanBox -> IO ()
dropMinChan ChanBox { .. } =
  case box of
    Chan { .. } -> do
      start <- getCompact <$> readCNFRef front
      let maxSize = B.length (getCompact vec)
          start' = (start + 1) `mod` maxSize
      c <- newCompactIn front start'
      writeCNFRef front c

pushMsg :: ChanBox -> Msg -> IO ()
pushMsg b@ChanBox { .. } msg =
  case box of
    Chan { .. } -> do
      size <- lengthChan box
      let maxSize = B.length (getCompact vec)
      when (size == maxSize - 1) $ dropMinChan b
      end <- getCompact <$> readCNFRef rear
      let end' = (end + 1) `mod` maxSize
          v = getCompact vec
          msg' = v ! end
      M.forM_ [0 .. 1023] $ \i -> do
        a <- V.read msg i
        c <- appendCompactNoShare vec a
        -- FIXME: unsafe interface
        V.write msg' i (getCompact c)
      c <- newCompactIn rear end'
      writeCNFRef rear c
