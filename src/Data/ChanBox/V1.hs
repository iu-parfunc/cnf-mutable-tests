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

newMessage :: ChanBox -> Int -> IO Msg
newMessage ChanBox { .. } n = V.replicate 1024 n

-- FIXME: CNF crashes with large size
maxLengthChan :: Int
maxLengthChan = 20000

(|+|) :: Int -> Int -> Int
x |+| y = (x + y) `mod` (maxLengthChan - 1)

newBox :: IO ChanBox
newBox = runCIO $ do
  front <- newCNFRef 0
  rear <- newCNFRef 0
  msg <- V.replicate 1024 0
  let vec = B.replicate maxLengthChan msg
  c <- liftIO $ newCompactIn front vec
  let box = Chan c front rear
  return $ ChanBox box

lengthChan :: Chan s -> IO Int
lengthChan Chan { .. } = do
  start <- getCompact <$> readCNFRef front
  end <- getCompact <$> readCNFRef rear
  return $ if end >= start
             then end - start
             else end - start + maxLengthChan + 1

sizeBox :: ChanBox -> IO Int
sizeBox ChanBox { .. } = lengthChan box

dropMinChan :: ChanBox -> IO ()
dropMinChan ChanBox { .. } =
  case box of
    Chan { .. } -> do
      start <- getCompact <$> readCNFRef front
      -- end <- getCompact <$> readCNFRef rear
      let start' = start |+| 1
      c <- newCompactIn front start'
      writeCNFRef front c

pushMsg :: ChanBox -> Msg -> IO ()
pushMsg b@ChanBox { .. } msg =
  case box of
    Chan { .. } -> do
      sz <- lengthChan box
      when (sz == maxLengthChan) $ dropMinChan b
      -- start <- getCompact <$> readCNFRef front
      end <- getCompact <$> readCNFRef rear
      let end' = end |+| 1
          v = getCompact vec
          msg' = v ! end
      M.forM_ [0 .. 1023] $ \i -> do
        a <- unsafeRead msg i
        c <- appendCompactNoShare vec a
        -- FIXME: unsafe interface
        unsafeWrite msg' i (getCompact c)
      c <- newCompactIn rear end'
      writeCNFRef rear c
