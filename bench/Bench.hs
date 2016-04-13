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

import qualified Data.CList        as CL
import qualified Data.CList.NoFree as CLNF

data Env = Env { n          :: Int
               , k          :: Int
               , size       :: Int
               , range      :: Int64
               , criterion  :: [String]
               , randomInts :: Vector Int64
               }
  deriving (Eq, Show, Data, Typeable, Generic, NFData)

defaultEnv :: Env
defaultEnv = Env
  { n = (10 ^ 3) &= name "N" &= help "Number of phase 1 operations"
  , k = (10 ^ 3) &= name "K" &= help "Number of phase 2 operations"
  , size = (10 ^ 7) &= help "Size of precomputed random vector"
  , range = (2 ^ 64) &= help "Range for random values"
  , criterion = [] &= help "Arguments to criterion"
  , randomInts = empty &= ignore
  } &= CA.verbosity &= program "bench"

setupEnv :: Env -> IO Env
setupEnv env@Env { .. } = do
  gen <- PCG.createSystemRandom
  randomInts <- VU.replicateM size (uniformR (0, range) gen :: IO Int64)
  return $ env { randomInts = randomInts }

clistBench :: Env -> IO ()
clistBench Env { .. } = do
  cl :: CL.CList Int64 <- CL.newCList
  for_ 1 n $ \i -> do
    CL.pushCList cl $ randomInts ! (i `mod` size)
  for_ 1 k $ \i -> do
    CL.popCList cl
    CL.pushCList cl $ randomInts ! (i `mod` size)
  void $ CL.readCList cl

clistnfBench :: Env -> IO ()
clistnfBench Env { .. } = do
  cl :: CLNF.CList Int64 <- CLNF.newCList
  for_ 1 n $ \i -> do
    CLNF.pushCList cl $ randomInts ! (i `mod` size)
  for_ 1 k $ \i -> do
    CLNF.popCList cl
    CLNF.pushCList cl $ randomInts ! (i `mod` size)
  void $ CLNF.readCList cl

config :: Config
config = defaultConfig { reportFile = Just "report.html"
                       , csvFile = Just "report.csv"
                       }

main :: IO ()
main = do
  args@Env{ .. } <- cmdArgs defaultEnv
  withArgs criterion $ defaultMainWith config
    [ env (setupEnv args) $
      \env -> bgroup "CList"
              [ bgroup "Data.CList" [ bench "2-phase" $ nfIO (clistBench env) ]
              , bgroup "Data.CList.NoFree" [ bench "2-phase" $ nfIO (clistnfBench env) ]
              ]
    ]
