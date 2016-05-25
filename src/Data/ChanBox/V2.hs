{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE Strict                    #-}
{-# LANGUAGE StrictData                #-}

-- |

module Data.ChanBox.V2 where

import Control.DeepSeq
import Control.Monad
import Data.CNFRef
import Data.CNFRef.DeepStrict
import Data.Compact.Indexed
import Data.Vector.Unboxed.Mutable as V
import GHC.Generics

type Msg = IOVector Int

data MsgList s where
  Nil :: BlockChain s -> MsgList s
  Cons :: Compact s Msg -> CNFRef s (MsgList s) -> MsgList s

instance NFData a => NFData (Compact s a) where
  rnf _ = ()
instance DeepStrict a => DeepStrict (Compact s a) where

deriving instance Generic (MsgList s)
deriving instance NFData (MsgList s)
deriving instance DeepStrict (MsgList s)

data Chan s = Chan { block :: BlockChain s, front :: MsgList s, rear :: MsgList s }
  deriving (Generic, NFData, DeepStrict)

type ChanRef s = CNFRef s (Chan s)

data ChanBox = forall s. ChanBox { box :: ChanRef s, free :: ChanRef s }

newMessage :: ChanBox -> Int -> IO Msg
newMessage ChanBox { .. } n = do
  chan@(Chan _ front _) <- getCompact <$> readCNFRef free
  case front of
    Nil _ -> V.replicate 1024 n
    Cons msg ref -> do
      let vec = getCompact msg
      forM_ [0 .. 1023] $ V.write vec n
      chan' <- tailChan chan
      c <- newCompactIn ref chan'
      writeCNFRef free c
      return vec

newBox :: IO ChanBox
newBox = runCIO $ do
  bl <- getBlockChain
  boxRef <- newCNFRef $ Chan bl (Nil bl) (Nil bl)
  freeRef <- newCNFRef $ Chan bl (Nil bl) (Nil bl)
  return $ ChanBox boxRef freeRef

lengthChan :: Chan s -> IO Int
lengthChan Chan { .. } = go front
  where
    go p =
      case p of
        Nil _ -> return 0
        Cons _ ref -> do
          c <- readCNFRef ref
          l <- go (getCompact c)
          return $ l + 1

sizeBox :: ChanBox -> IO Int
sizeBox ChanBox { .. } = do
  boxChan <- getCompact <$> readCNFRef box
  freeChan <- getCompact <$> readCNFRef free
  liftM2 (+) (lengthChan boxChan) (lengthChan freeChan)

headChan :: Chan s -> IO (Compact s Msg)
headChan Chan { .. } =
  case front of
    Nil _      -> error "head on empty list"
    Cons msg _ -> return msg

tailChan :: Chan s -> IO (Chan s)
tailChan Chan { .. } =
  case front of
    Nil _ -> error "tail on empty list"
    Cons _ ref -> do
      p <- getCompact <$> readCNFRef ref
      return $ Chan block p rear

consChan :: Compact s Msg -> ChanRef s -> IO ()
consChan msg ref = do
  Chan block front rear <- getCompact <$> readCNFRef ref
  p <- newCNFRefIn block front
  let front' = Cons msg p
      rear' =
        case rear of
          Nil _ -> front
          r     -> r
  c <- newCompactIn ref $ Chan block front' rear'
  writeCNFRef ref c

snocChan :: ChanRef s -> Compact s Msg -> IO ()
snocChan ref msg = do
  Chan block front rear <- getCompact <$> readCNFRef ref
  p <- newCNFRefIn block (Nil block)
  let rear' = Cons msg p
      front' =
        case front of
          Nil _ -> rear'
          f     -> f
  case rear of
    Nil _ -> do
      c' <- newCompactIn ref $ Chan block front' rear'
      writeCNFRef ref c'
    Cons _ p -> do
      c' <- newCompactIn p rear'
      writeCNFRef p c'

maxLengthChan :: Int
maxLengthChan = 200000

dropMinChan :: ChanBox -> IO ()
dropMinChan ChanBox { .. } = do
  boxChan <- getCompact <$> readCNFRef box
  l <- lengthChan boxChan
  when (l >= maxLengthChan) $ do
    v <- headChan boxChan
    boxChan' <- tailChan boxChan
    c <- newCompactIn box boxChan'
    writeCNFRef box c
    consChan v free

pushMsg :: ChanBox -> Msg -> IO ()
pushMsg b@ChanBox { .. } msg = do
  -- dropMinChan b
  c <- newCompactIn box msg
  snocChan box c
