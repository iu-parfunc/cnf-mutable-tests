{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.DeepSeq
import Control.Monad
import Criterion.Main
import Criterion.Types
import Data.Vector.Unboxed         as VU
import GHC.Generics
import GHC.Int
import System.Console.CmdArgs      as CA
import System.Environment
import System.Random.PCG.Fast.Pure as PCG
import Utils

import qualified Data.ChanBox.V1 as CB1
import qualified Data.ChanBox.V2 as CB2

data Env = Env { n          :: Int
               , range      :: Int
               , size       :: Int
               , criterion  :: [String]
               , randomInts :: Vector Int
               }
  deriving (Eq, Show, Data, Typeable, Generic, NFData)

defaultEnv :: Env
defaultEnv = Env
  { n = (10 ^ 5) &= name "N" &= help "Number of messages"
  , range = (2 ^ 63 - 1) &= help "Integer range for messages"
  , size = (10 ^ 7) &= help "Size of precomputed random vector"
  , criterion = [] &= help "Arguments to criterion"
  , randomInts = empty &= ignore
  } &= CA.verbosity &= program "bench-chanbox"

setupEnv :: Env -> IO Env
setupEnv env@Env { .. } = do
  gen <- PCG.createSystemRandom
  randomInts <- VU.replicateM size (uniformR (0, range) gen :: IO Int)
  return $ env { randomInts = randomInts }

config :: Config
config = defaultConfig { reportFile = Just "report.html"
                       , csvFile = Just "report.csv"
                       }

cbv1Bench :: Env -> IO ()
cbv1Bench Env { .. } = do
  cb <- CB1.newBox
  for_ 1 n $ \i -> do
    msg <- CB1.newMessage cb $ randomInts ! (i `mod` size)
    CB1.pushMsg cb msg

main :: IO ()
main = do
  args@Env { .. } <- cmdArgs defaultEnv
  withArgs criterion $
    defaultMainWith config
      [ env (setupEnv args) $ \env ->
        bgroup "ChanBox" [bgroup "Data.ChanBox.V1" [bench (show n) $ nfIO (cbv1Bench env)]]
      ]
