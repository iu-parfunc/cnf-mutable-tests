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

module Data.ChanBox where

import           Control.DeepSeq
import           Control.Monad
import           Data.CNFRef
import           Data.CNFRef.DeepStrict
import           Data.Compact.Indexed
import qualified Data.Map.Strict             as Map
import           Data.Vector.Unboxed.Mutable as V
import           Data.Word
import           GHC.Generics

data Msg = Msg Int (IOVector Int)
  deriving (Generic, NFData, DeepStrict)

data Chan s where
  Nil :: BlockChain s -> Chan s
  Cons :: Compact s Msg -> CNFRef s (Chan s) -> Chan s

instance NFData (Compact s a) where
  rnf _ = ()

deriving instance Generic (Chan s)
deriving instance NFData (Chan s)
deriving instance DeepStrict (Chan s)

data ChanBox = forall s. ChanBox { box  :: CNFRef s (Chan s)
                                 , free :: CNFRef s (Chan s)
                                 }

newMessage :: CNFRef s (Chan s) -> Int -> IO (Compact s Msg)
newMessage free n = do
  c <- readCNFRef free
  case getCompact c of
    Nil bl -> do
      vec <- V.replicate 1024 n
      appendCompact bl $ Msg n vec
    Cons msg ref -> do
      let Msg _ vec = getCompact msg
      forM_ [0 .. 1023] $ V.write vec n
      c' <- newCompactIn free $ Msg n vec
      p <- readCNFRef ref
      writeCNFRef free p
      return c'

newBox :: IO ChanBox
newBox = runCIO $ do
  bl <- getBlockChain
  box <- newCNFRef (Nil bl)
  free <- newCNFRef (Nil bl)
  return $ ChanBox box free

lengthChan :: Chan s -> IO Int
lengthChan chan =
  case chan of
    Nil _ -> return 0
    Cons _ ref -> do
      c <- readCNFRef ref
      l <- lengthChan $ getCompact c
      return $ l + 1

deleteMinChan :: ChanBox -> IO ()
deleteMinChan ChanBox { .. } = do
  c <- readCNFRef box
  let chan = getCompact c
  case chan of
    Nil _ -> return ()
    Cons c ref ->
      let Msg n _ = getCompact c
      in go n chan ref

  where
    go n chan ref = do
      c <- readCNFRef ref
      let chan' = getCompact c
      case chan' of
        Nil _ -> return ()
        Cons c ref -> do
          let Msg n' _ = getCompact c
          if n < n'
            then go n chan ref
            else go n' chan' ref

pushMsg :: ChanBox -> Msg  -> IO ()
pushMsg b@ChanBox { .. } msg = do
  c <- newCompactIn box msg
  let chan = Cons c box
  l <- lengthChan chan
  when (200000 < l) $
    deleteMinChan b
  c' <- newCompactIn box chan
  writeCNFRef box c'
