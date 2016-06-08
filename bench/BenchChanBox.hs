{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import           System.Console.CmdArgs      as CA
import           System.Environment
import           System.Random.PCG.Fast.Pure as PCG

import qualified Data.ChanBox.V0 as CB0
import qualified Data.ChanBox.V1 as CB1
import qualified Data.ChanBox.V2 as CB2

data Flags = Flags { size       :: Int
                   , threads    :: Int
                   , range      :: Int
                   , randomSize :: Int
                   , criterion  :: [String]
                   }
  deriving (Eq, Show, Data, Typeable, Generic, NFData)

data Env = Env { randomInts :: VU.Vector Int
               , cb0        :: CB0.ChanBox
               , cb1        :: CB1.ChanBox
               , cb2        :: CB2.ChanBox
               , cb0s       :: V.Vector CB0.ChanBox
               , cb1s       :: V.Vector CB1.ChanBox
               , cb2s       :: V.Vector CB2.ChanBox
               }
  deriving (Generic, NFData)

defaultFlags :: Flags
defaultFlags = Flags
  { size = (10 ^ 2) &= name "size" &= help "Maximum size of ChanBox"
  , threads = 4 &= name "threads" &= help "Number of threads for the concurrent benchmark"
  , range = (2 ^ 63 - 1) &= help "Integer range for messages"
  , randomSize = (10 ^ 7) &= help "Size of precomputed random vector"
  , criterion = [] &= help "Arguments to criterion"
  } &= CA.verbosity &= program "bench-chanbox"

setupEnv :: Flags -> IO Env
setupEnv Flags { .. } = do
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

cbv0Bench :: Flags -> Env -> Benchmarkable
cbv0Bench Flags { .. } Env { .. } = Benchmarkable $
  \n -> for_ 0 (fromIntegral n) $ \i -> do
    let cb = cb0
    msg <- CB0.newMessage cb $ randomInts VU.! (i `mod` randomSize)
    CB0.pushMsg cb msg

cbv1Bench :: Flags -> Env -> Benchmarkable
cbv1Bench Flags { .. } Env { .. } = Benchmarkable $
  \n -> for_ 1 (fromIntegral n) $ \i -> do
    let cb = cb1
    msg <- CB1.newMessage cb $ randomInts VU.! (i `mod` randomSize)
    CB1.pushMsg cb msg

cbv2Bench :: Flags -> Env -> Benchmarkable
cbv2Bench Flags { .. } Env { .. } = Benchmarkable $
  \n -> for_ 1 (fromIntegral n) $ \i -> do
    let cb = cb2
    msg <- CB2.newMessage cb $ randomInts VU.! (i `mod` randomSize)
    CB2.pushMsg cb msg

cbv0ConcBench :: Flags -> Env -> Benchmarkable
cbv0ConcBench Flags { .. } Env { .. } = Benchmarkable $
  \n -> forConc_ 0 (threads - 1) $ \t -> do
    let cb = cb0s V.! t
    for_ 0 (fromIntegral n) $ \i -> do
      msg <- CB0.newMessage cb $ randomInts VU.! (i `mod` randomSize)
      CB0.pushMsg cb msg

cbv1ConcBench :: Flags -> Env -> Benchmarkable
cbv1ConcBench Flags { .. } Env { .. } = Benchmarkable $
  \n -> forConc_ 0 (threads - 1) $ \t -> do
    let cb = cb1s V.! t
    for_ 0 (fromIntegral n) $ \i -> do
      msg <- CB1.newMessage cb $ randomInts VU.! (i `mod` randomSize)
      CB1.pushMsg cb msg

cbv2ConcBench :: Flags -> Env -> Benchmarkable
cbv2ConcBench Flags { .. } Env { .. } = Benchmarkable $
  \n -> forConc_ 0 (threads - 1) $ \t -> do
    let cb = cb2s V.! t
    for_ 0 (fromIntegral n) $ \i -> do
      msg <- CB2.newMessage cb $ randomInts VU.! (i `mod` randomSize)
      CB2.pushMsg cb msg

main :: IO ()
main = do
  flags@Flags { .. } <- cmdArgs defaultFlags
  config <- getConfig
  withArgs criterion $
    defaultMainWith config
      [ env (setupEnv flags) $ \env ->
        bgroup "ChanBox"
          [ bench "V0" $ cbv0Bench flags env
          , bench "V1" $ cbv1Bench flags env
          , bench "V2" $ cbv2Bench flags env
          , bgroup "Concurrent"
              [ bench "V0" $ cbv0ConcBench flags env
              , bench "V1" $ cbv1ConcBench flags env
              , bench "V2" $ cbv2ConcBench flags env
              ]
          ]
      ]
