{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE StrictData          #-}

module Main where

import Control.DeepSeq
import Control.Monad
import Criterion.Main
import Criterion.Types
import Data.CList
import Data.Vector.Unboxed         as VU
import GHC.Generics
import GHC.Int
import System.Random.PCG.Fast.Pure as PCG
import Utils

data Env = Env { n          :: Int
               , k          :: Int
               , size       :: Int
               , range      :: Int64
               , randomInts :: Vector Int64
               } deriving (Generic, NFData)

setupEnv :: IO Env
setupEnv = do
  -- Get from args
  let n = 10 ^ 3
      k = 10 ^ 3
      size = 10 ^ 7
      range = 2 ^ 64
  gen <- PCG.createSystemRandom
  randomInts <- VU.replicateM size (uniformR (0, range) gen :: IO Int64)
  return $ Env n k size range randomInts

clistBench :: Env -> IO ()
clistBench Env { .. } = do
  cl :: CList Int64 <- newCList
  for_ 1 n $ \i -> do
    pushCList cl $ randomInts ! (i `mod` size)
  for_ 1 k $ \i -> do
    popCList cl
    pushCList cl $ randomInts ! (i `mod` size)
  void $ readCList cl

config :: Config
config = defaultConfig { reportFile = Just "report.html"
                       , csvFile = Just "report.csv"
                       }

main :: IO ()
main =
  defaultMainWith config
    [ env setupEnv $
      \ ~env -> bgroup "CList"
                [ bench "2-phase" $ nfIO (clistBench env) ]
    ]
