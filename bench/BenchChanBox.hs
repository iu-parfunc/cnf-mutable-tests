{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Utils
import Criterion.Main
import Criterion.Types
import Data.Vector.Unboxed         as VU
import GHC.Generics
import GHC.Int
import System.Console.CmdArgs      as CA
import System.Environment
import System.Random.PCG.Fast.Pure as PCG

import qualified Data.ChanBox.V0 as CB0
import qualified Data.ChanBox.V1 as CB1
import qualified Data.ChanBox.V2 as CB2

data Flags = Flags { size       :: Int
                   , range      :: Int
                   , randomSize :: Int
                   , criterion  :: [String]
                   }
  deriving (Eq, Show, Data, Typeable, Generic, NFData)

data Env = Env { randomInts :: VU.Vector Int
               , cb0        :: CB0.ChanBox
               , cb1        :: CB1.ChanBox
               }
  deriving (Generic, NFData)

defaultFlags :: Flags
defaultFlags = Flags
  { size = (10 ^ 3) &= name "size" &= help "Maximum size of ChanBox"
  , range = (2 ^ 63 - 1) &= help "Integer range for messages"
  , randomSize = (10 ^ 7) &= help "Size of precomputed random vector"
  , criterion = [] &= help "Arguments to criterion"
  } &= CA.verbosity &= program "bench-chanbox"

setupEnv :: Flags -> IO Env
setupEnv Flags { .. } = do
  gen <- PCG.createSystemRandom
  randomInts <- VU.replicateM randomSize (uniformR (0, range) gen :: IO Int)
  cb0 <- CB0.newBox
  cb1 <- CB1.newBox' size
  return $ Env { randomInts = randomInts, cb0 = cb0, cb1 = cb1 }

config :: Config
config = defaultConfig { reportFile = Just "report.html"
                       , csvFile = Just "report.csv"
                       }

cbv0Bench :: Flags -> Env -> Benchmarkable
cbv0Bench Flags { .. } Env { .. } = Benchmarkable $
  \n -> for_ 0 (fromIntegral n) $ \i -> do
    msg <- CB0.newMessage cb0 $ randomInts ! (i `mod` randomSize)
    CB0.pushMsg cb0 msg

cbv1Bench :: Flags -> Env -> Benchmarkable
cbv1Bench Flags { .. } Env { .. } = Benchmarkable $
  \n -> for_ 1 (fromIntegral n) $ \i -> do
    msg <- CB1.newMessage cb1 $ randomInts ! (i `mod` randomSize)
    CB1.pushMsg cb1 msg

main :: IO ()
main = do
  flags@Flags { .. } <- cmdArgs defaultFlags
  withArgs criterion $
    defaultMainWith config
      [ env (setupEnv flags) $ \env ->
        bgroup "ChanBox"
          [ bench "Data.ChanBox.V0" $ cbv0Bench flags env
          , bench "Data.ChanBox.V1" $ cbv1Bench flags env
          ]
      ]
