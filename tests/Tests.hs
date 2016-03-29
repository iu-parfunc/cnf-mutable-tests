{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Control.DeepSeq
import Data.Foldable
import GHC.Prim
import System.IO.Unsafe
import System.Mem
import Test.Tasty
import Test.Tasty.HUnit

import           Data.CList                  as CL
import           Data.Compact
import           Data.IntBox                 as IB
import           Data.IORef
import           Data.Primitive.MutVar
import qualified Data.Vector.Mutable         as V
import qualified Data.Vector.Unboxed.Mutable as U

instance NFData a => NFData (MutVar RealWorld a) where
  rnf a = unsafePerformIO $ modifyMutVar' a force

instance NFData a => NFData (V.IOVector a) where
  rnf a = unsafePerformIO $ modifyIOVector' a force
    where modifyIOVector' !a !f = go' a f 0 (V.length a)
          go' !a !f !i !l = if i < l
                               then V.unsafeModify a f i >>= \ !_ -> go' a f (i + 1) l
                               else return ()

printV :: Show a => V.IOVector a -> IO ()
printV a = go 0 (V.length a)
  where go i l = if i < l
                    then do x <- V.read a i
                            print x
                            go (i + 1) l
                    else return ()

readV :: V.IOVector a -> IO [a]
readV a = go a 0 (V.length a)
  where go a i l = if i < l
                    then do x <- V.read a i
                            xs <- go a (i + 1) l
                            return (x:xs)
                    else return []

readU :: U.Unbox a => U.IOVector a -> IO [a]
readU a = go a 0 (U.length a)
  where go a i l = if i < l
                    then do x <- U.read a i
                            xs <- go a (i + 1) l
                            return (x:xs)
                    else return []

tests :: TestTree
tests =
    testGroup
        "Tests"
        [ compactTests
        , iorefTests
        , mutvarTests
        , uiovectorTests
        , intboxTests
        , clistTests
        -- crashes
        -- , iovectorTests
        ]

compactTests =
    testGroup
        "Pure value in a compact"
        [ testCase "Int in a compact" $
          do c <- newCompact 64 (42 :: Int)
             c' <- appendCompact c (21 :: Int)
             let x = getCompact c'
             x @?= 21]

iorefTests =
    testGroup
        "IORef in a compact"
        [ testCase "IORef Int in a compact" $
          do x <- newIORef (42 :: Int)
             c <- newCompact 64 x
             y <- newIORef (21 :: Int)
             c' <- appendCompact c y
             z <- readIORef $ getCompact c'
             z @?= 21]

mutvarTests =
    testGroup
        "MutVar in a compact"
        [ testCase "MutVar Int in a compact" $
          do x <- newMutVar (42 :: Int)
             c <- newCompact 64 x
             y <- newMutVar (21 :: Int)
             c' <- appendCompact c y
             z <- readMutVar $ getCompact c'
             z @?= 21]

uiovectorTests =
    testGroup
        "Unboxed Mutable Vector in a compact"
        [ testCase "U.IOVector Int in a compact" $
          do x :: U.IOVector Int <- U.new 5
             _ <- U.set x 42
             c <- newCompact 64 x
             y <- U.new 5
             _ <- U.set y 21
             c' <- appendCompact c y
             z :: [Int] <- readU (getCompact c')
             z @?= replicate 5 21]

iovectorTests =
    testGroup
        "Boxed Mutable Vector in a compact"
        [ testCase "V.IOVector Int in a compact" $
          do x :: V.IOVector Int <- V.new 5
             _ <- V.set x 42
             c <- newCompact 64 x
             y <- V.new 5
             _ <- V.set y 21
             c' <- appendCompact c y
             z :: [Int] <- readV (getCompact c')
             z @?= replicate 5 21]

intboxTests =
    testGroup
        "IntBox"
        [ testCase "IntBox" $
          do ib <- newIntBox
             let vs :: [Int] = [1 .. 100]
             forM_ vs $ writeIntBox ib
             n <- readIntBox ib
             n @?= vs]

clistTests =
    testGroup
        "CList"
        [ testCase "writeCList" $
          do cl <- newCList
             forM_ vs $ writeCList cl
             n <- readCList cl
             n @?= vs
        , testCase "popCList" $
          do cl <- newCList
             forM_ vs $ writeCList cl
             forM_ vs . const $ popCList cl
             n <- readCList cl
             n @?= []]
  where
    vs :: [Int]
    vs = [1 .. 100]

main :: IO ()
main = defaultMain tests
