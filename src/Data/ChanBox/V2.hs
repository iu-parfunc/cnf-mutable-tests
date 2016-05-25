{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE Strict                    #-}
{-# LANGUAGE StrictData                #-}
{-# LANGUAGE TypeSynonymInstances      #-}

-- |

module Data.ChanBox.V2 where

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Trans
import           Data.CNFRef
import           Data.CNFRef.DeepStrict
import           Data.Compact.Indexed
import qualified Data.Map.Strict             as Map
import           Data.Vector.Unboxed.Mutable as V
import           Data.Word
import           GHC.Generics

type Msg = IOVector Int

data MsgList s where
  Nil :: BlockChain s -> MsgList s
  Cons :: Compact s Msg -> CNFRef s (MsgList s) -> MsgList s

instance NFData a => NFData (Compact s a) where
  rnf _ = ()

deriving instance Generic (MsgList s)
deriving instance NFData (MsgList s)
deriving instance DeepStrict (MsgList s)

data Chan s = Chan { front :: CNFRef s (MsgList s), rear :: CNFRef s (MsgList s) }
  deriving (Generic, NFData, DeepStrict)

data ChanBox = forall s. ChanBox { box :: Chan s, free :: Chan s }

newMessage :: ChanBox -> Int -> IO Msg
newMessage (ChanBox _ (Chan front _)) n = do
  c <- readCNFRef front
  case getCompact c of
    Nil _ -> V.replicate 1024 n
    Cons msg ref -> do
      let vec = getCompact msg
      forM_ [0 .. 1023] $ V.write vec n
      p <- readCNFRef ref
      writeCNFRef front p
      return vec

newBox :: IO ChanBox
newBox = runCIO $ do
  bl <- getBlockChain
  box <- newCNFRef (Nil bl)
  free <- newCNFRef (Nil bl)
  return $ ChanBox (Chan box box) (Chan free free)

lengthMsgList :: MsgList s -> IO Int
lengthMsgList ls =
  case ls of
    Nil _ -> return 0
    Cons _ ref -> do
      c <- readCNFRef ref
      l <- lengthMsgList $ getCompact c
      return $ l + 1

headMsgList :: MsgList s -> IO (Compact s Msg)
headMsgList ls =
  case ls of
    Nil _      -> error "head on empty list"
    Cons msg _ -> return msg

tailMsgList :: MsgList s -> IO (MsgList s)
tailMsgList ls =
  case ls of
    Nil _      -> error "tail on empty list"
    Cons _ ref -> getCompact <$> readCNFRef ref

lengthChan :: Chan s -> IO Int
lengthChan Chan { .. } = do
  c <- readCNFRef front
  lengthMsgList $ getCompact c

sizeBox :: ChanBox -> IO Int
sizeBox ChanBox { .. } =
  liftM2 (+) (lengthChan box) (lengthChan free)

maxLengthChan :: Int
maxLengthChan = 200000

dropMinChan :: ChanBox -> IO ()
dropMinChan ChanBox { .. } = do
  c <- readCNFRef $ front box
  let boxMsgs = getCompact c
  l <- lengthMsgList boxMsgs
  when (l >= maxLengthChan) $ do
    v <- headMsgList boxMsgs
    boxMsgs' <- tailMsgList boxMsgs
    let freeMsgs' = Cons v $ front free
    c' <- appendCompactNoShare c freeMsgs'
    writeCNFRef (front free) c'
    c' <- appendCompactNoShare c boxMsgs'
    writeCNFRef (front box) c'

pushMsg :: ChanBox -> Msg -> IO ()
pushMsg b@ChanBox { .. } msg = do
  -- FIXME:
  bl <- newCompactIn (rear box) ()
  c <- appendCompactNoShare bl msg
  p <- readCNFRef (rear box)
  case getCompact p of
    Nil bl -> do
      let rear' = Cons c (rear box)
      c' <- appendCompactNoShare bl rear'
      writeCNFRef (front box) c'
      writeCNFRef (rear box) c'
    Cons msg ref -> do
      -- dropMinChan b
      ref' <- newCNFRefIn' (rear box) (Nil bl)
      let rear' = Cons c ref'
      c' <- appendCompactNoShare bl rear'
      writeCNFRef ref c'
      writeCNFRef (rear box) c'
