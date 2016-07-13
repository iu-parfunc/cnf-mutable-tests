{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE StrictData          #-}

module Main where

import           Control.DeepSeq
import           Control.Monad.Utils
import           Criterion.Main
import           Criterion.Types
import           Data.Time
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as VU
import           GHC.Generics
import           Network.HostName
import           System.Environment
import           System.IO.Unsafe
import           System.Random.PCG.Fast.Pure as PCG

import qualified Data.ChanBox.V0 as CB0
import qualified Data.ChanBox.V1 as CB1
import qualified Data.ChanBox.V2 as CB2

{-# NOINLINE size #-}
size :: Int
size = unsafePerformIO $ read <$> getEnv "SIZE"

{-# NOINLINE threads #-}
threads :: Int
threads = unsafePerformIO $ read <$> getEnv "THREADS"

range :: Int
range = maxBound

randomSize :: Int
randomSize = 10 ^ 7

data Env = Env { randomInts :: VU.Vector Int
               , cb0        :: CB0.ChanBox
               , cb1        :: CB1.ChanBox
               , cb2        :: CB2.ChanBox
               , cb0s       :: V.Vector CB0.ChanBox
               , cb1s       :: V.Vector CB1.ChanBox
               , cb2s       :: V.Vector CB2.ChanBox
               }
  deriving (Generic, NFData)

setupEnv :: IO Env
setupEnv = do
  gen <- PCG.createSystemRandom
  randomInts <- VU.replicateM randomSize (uniformR (0, range) gen :: IO Int)
  cb0 <- CB0.newBox size
  cb1 <- CB1.newBox size
  cb2 <- CB2.newBox size
  cb0s <- V.replicateM threads (CB0.newBox size)
  cb1s <- V.replicateM threads (CB1.newBox size)
  cb2s <- V.replicateM threads (CB2.newBox size)
  return $ Env randomInts cb0 cb1 cb2 cb0s cb1s cb2s

getConfig :: IO Config
getConfig = do
  hostname <- getHostName
  time <- getCurrentTime
  let date = formatTime defaultTimeLocale (iso8601DateFormat Nothing) time
      filename = "report-" ++ hostname ++ "-" ++ date
  return $ defaultConfig
    { reportFile = Just $ filename ++ ".html"
    , csvFile = Just $ filename ++ ".csv"
    }

cbv0Bench :: Env -> Benchmarkable
cbv0Bench Env { .. } = Benchmarkable $
  \n -> for_ 0 (fromIntegral n) $ \i -> do
    let cb = cb0
    msg <- CB0.newMessage cb $ randomInts VU.! (i `mod` randomSize)
    CB0.pushMsg cb msg

cbv1Bench :: Env -> Benchmarkable
cbv1Bench Env { .. } = Benchmarkable $
  \n -> for_ 1 (fromIntegral n) $ \i -> do
    let cb = cb1
    msg <- CB1.newMessage cb $ randomInts VU.! (i `mod` randomSize)
    CB1.pushMsg cb msg

cbv2Bench :: Env -> Benchmarkable
cbv2Bench Env { .. } = Benchmarkable $
  \n -> for_ 1 (fromIntegral n) $ \i -> do
    let cb = cb2
    msg <- CB2.newMessage cb $ randomInts VU.! (i `mod` randomSize)
    CB2.pushMsg cb msg

cbv0ConcBench :: Env -> Benchmarkable
cbv0ConcBench Env { .. } = Benchmarkable $
  \n -> forConc_ 0 (threads - 1) $ \t -> do
    let cb = cb0s V.! t
    for_ 0 (fromIntegral n) $ \i -> do
      msg <- CB0.newMessage cb $ randomInts VU.! (i `mod` randomSize)
      CB0.pushMsg cb msg

cbv1ConcBench :: Env -> Benchmarkable
cbv1ConcBench Env { .. } = Benchmarkable $
  \n -> forConc_ 0 (threads - 1) $ \t -> do
    let cb = cb1s V.! t
    for_ 0 (fromIntegral n) $ \i -> do
      msg <- CB1.newMessage cb $ randomInts VU.! (i `mod` randomSize)
      CB1.pushMsg cb msg

cbv2ConcBench :: Env -> Benchmarkable
cbv2ConcBench Env { .. } = Benchmarkable $
  \n -> forConc_ 0 (threads - 1) $ \t -> do
    let cb = cb2s V.! t
    for_ 0 (fromIntegral n) $ \i -> do
      msg <- CB2.newMessage cb $ randomInts VU.! (i `mod` randomSize)
      CB2.pushMsg cb msg

main :: IO ()
main = do
  config <- getConfig
  defaultMainWith config
    [ env setupEnv $ \ ~env ->
      bgroup "ChanBox"
        [ bench "V0" $ cbv0Bench env
        , bench "V1" $ cbv1Bench env
        , bench "V2" $ cbv2Bench env
        , bgroup "Concurrent"
            [ bench "V0" $ cbv0ConcBench env
            , bench "V1" $ cbv1ConcBench env
            , bench "V2" $ cbv2ConcBench env
            ]
        ]
    ]
