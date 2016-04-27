{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE Strict                    #-}
{-# LANGUAGE StrictData                #-}
{-# LANGUAGE TypeSynonymInstances      #-}

-- |

module Main where

import qualified Control.Exception      as Exception
import qualified Control.Monad          as Monad
import qualified Data.ByteString        as ByteString
import           Data.CNFRef
import           Data.CNFRef.DeepStrict
import           Data.Compact.Indexed
import qualified Data.Map.Strict        as Map

data Msg = Msg Int ByteString.ByteString

type Chan = Map.Map Int ByteString.ByteString

data ChanBox = forall s. ChanBox { box :: CNFRef s Chan }

instance DeepStrict Chan where

message :: Int -> Msg
message n = Msg n (ByteString.replicate 1024 (fromIntegral n))

pushMsg :: ChanBox -> Msg -> IO ()
pushMsg ChanBox { .. } (Msg msgId msgContent) = do
  chan <- readCNFRef box
  chan' <- Exception.evaluate $
             let inserted = Map.insert msgId msgContent $ getCompact chan
             in if 200000 < Map.size inserted
                  then Map.deleteMin inserted
                  else inserted
  c <- appendCompact box chan'
  writeCNFRef box c

newBox :: IO ChanBox
newBox = runCIO $ do
  chan <- newCNFRef Map.empty
  return $ ChanBox chan

main :: IO ()
main = do
  b <- newBox
  Monad.forM_ [1 .. 10000] $
    \i -> pushMsg b (message i)
