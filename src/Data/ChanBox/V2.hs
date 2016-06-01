{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE Strict                    #-}
{-# LANGUAGE StrictData                #-}

-- |

module Data.ChanBox.V2 where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Utils
import Data.CNFRef
import Data.CNFRef.DeepStrict
import Data.Compact.Indexed
import Data.Vector.Unboxed.Mutable as VU
import GHC.Generics

type Msg = IOVector Int

data MsgList s = Nil (BlockChain s)
               | Cons (Compact s Msg) (MsgList s)
  deriving (Generic, NFData, DeepStrict)

data Chan s = Chan { front :: CNFRef s (MsgList s)
                   , rear  :: CNFRef s (MsgList s)
                   , size  :: CNFRef s Int
                   }
  deriving (Generic, NFData, DeepStrict)

data ChanBox = forall s. ChanBox { box     :: Chan s
                                 , free    :: Chan s
                                 , maxSize :: Int
                                 }

instance NFData ChanBox where
  rnf ChanBox { .. } = rnf box `seq` rnf free `seq` rnf maxSize

newMessage :: ChanBox -> Int -> IO Msg
newMessage ChanBox { .. } n = do
  m <- dropChan free
  case m of
    Nothing -> do
      msg <- VU.replicate 1024 n
      getCompact <$> newCompactIn (front box) msg
    Just c -> do
      let msg' = getCompact c
      for_ 0 1023 $ \i -> unsafeWrite msg' i n
      c' <- newCompactIn (front box) msg'
      return $ getCompact c'

{-# INLINE newBox' #-}
newBox' :: IO ChanBox
newBox' = newBox 2000

newBox :: Int -> IO ChanBox
newBox maxSize = runCIO $ do
  bl <- getBlockChain
  ref <- newCNFRef (Nil bl)
  ref' <- newCNFRef (Nil bl)
  sz <- newCNFRef 0
  sz' <- newCNFRef 0
  let box = Chan ref ref sz
      free = Chan ref' ref' sz'
  return $ ChanBox box free maxSize


{-# INLINE sizeChan #-}
sizeChan :: Chan s -> IO Int
sizeChan Chan { .. } =
  getCompact <$> readCNFRef size

{-# INLINE sizeBox #-}
sizeBox :: ChanBox -> IO Int
sizeBox ChanBox { .. } =
  liftM2 (+) (sizeChan box) (sizeChan free)

pushChan :: Chan s -> Compact s Msg -> IO ()
pushChan Chan { .. } c = do
  r <- getCompact <$> readCNFRef rear
  case r of
    Nil _ -> do
      r' <- newCompactIn rear (Cons c r)
      writeCNFRef front r'
      writeCNFRef rear r'
    Cons _ end -> do
      r' <- newCompactIn rear (Cons c end)
      writeCNFRef rear r'
  sz <- getCompact <$> readCNFRef size
  sz' <- newCompactIn size (sz + 1)
  writeCNFRef size sz'

dropChan :: Chan s -> IO (Maybe (Compact s Msg))
dropChan Chan { .. } = do
  f <- getCompact <$> readCNFRef front
  case f of
    Nil _ -> return Nothing
    Cons c next -> do
      f' <- newCompactIn front next
      writeCNFRef front f'
      sz <- getCompact <$> readCNFRef size
      sz' <- newCompactIn size (sz - 1)
      writeCNFRef size sz'
      return $ Just c

{-# INLINE dropMinChan #-}
dropMinChan :: ChanBox -> IO ()
dropMinChan ChanBox { .. } = do
  m <- dropChan box
  case m of
    Nothing -> return ()
    Just c  -> pushChan free c

pushMsg :: ChanBox -> Msg -> IO ()
pushMsg b@ChanBox { .. } msg = do
  sz <- sizeChan box
  when (sz == maxSize - 1) $ dropMinChan b
  c <- newCompactIn (front box) msg
  pushChan box c
