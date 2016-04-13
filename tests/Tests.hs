{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE TypeFamilies        #-}

module Main where

import Data.Foldable
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.CList                  as CL
import           Data.CList.MList            as ML
import qualified Data.CList.NoFree           as CLNF
import           Data.CNFRef
import           Data.Compact
import           Data.IntBox                 as IB
import           Data.IORef
import           Data.Primitive.MutVar
import qualified Data.Vector.Mutable         as V
import qualified Data.Vector.Unboxed.Mutable as U

printV :: Show a => V.IOVector a -> IO ()
printV a = go 0 (V.length a)
  where go i l =
          if i < l
             then do x <- V.read a i
                     print x
                     go (i + 1) l
             else return ()

readV :: V.IOVector a -> IO [a]
readV a = go 0 (V.length a)
  where go i l =
          if i < l
             then do x <- V.read a i
                     xs <- go (i + 1) l
                     return (x : xs)
             else return []

readU :: U.Unbox a => U.IOVector a -> IO [a]
readU a = go 0 (U.length a)
  where go i l =
          if i < l
             then do x <- U.read a i
                     xs <- go (i + 1) l
                     return (x : xs)
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
        , mlistTests
        , clistTests
        , clistnfTests
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

mlistTests =
    testGroup
        "MList"
        [ testCase "newMList" $
          do m <- newCNFRef (Nil :: List Int)
             n <- readMList m
             n @?= []
        , testCase "updateMList" $
          do m <- newCNFRef (Nil :: List Int)
             forM_ vs $ (updateMList m =<<) . copyToCompact m
             n <- readMList m
             n @?= ws
        , testCase "appendMList" $
          do m <- newCNFRef (Nil :: List Int)
             forM_ vs $
                 \a ->
                      do vec <- newVec a
                         ref <- newIORef Nil
                         c <- copyToCompact m $ Cons vec ref
                         appendMList m c
             n <- readMList m
             n @?= vs
        , testCase "dropMList" $
          do m <- newCNFRef (Nil :: List Int)
             forM_ vs $ (updateMList m =<<) . copyToCompact m
             forM_ ws $ dropMList m
             n <- readMList m
             n @?= []
        , testCase "popMList" $
          do m <- newCNFRef (Nil :: List Int)
             forM_ vs $ (updateMList m =<<) . copyToCompact m
             forM_ ws . const $ popMList m
             n <- readMList m
             n @?= []
        , testCase "lengthMList" $
          do m <- newCNFRef (Nil :: List Int)
             forM_ vs $ (updateMList m =<<) . copyToCompact m
             n <- lengthMList m
             n @?= length ws]
  where
    vs :: [Int]
    vs = ws ++ ws
    ws = [1 .. 1000]

clistTests =
    testGroup
        "CList"
        [ testCase "newCList" $
          do cl :: CL.CList Int <- CL.newCList
             n <- CL.readCList cl
             n @?= []
        , testCase "pushCList" $
          do cl <- CL.newCList
             forM_ vs $ CL.pushCList cl
             n <- CL.readCList cl
             n @?= vs
        , testCase "popCList" $
          do cl <- CL.newCList
             forM_ vs $ CL.pushCList cl
             forM_ vs . const $ CL.popCList cl
             n <- CL.readCList cl
             n @?= []
        , testCase "sizeCList" $
          do cl <- CL.newCList
             forM_ vs $ CL.pushCList cl
             forM_ vs . const $ CL.popCList cl
             n <- CL.sizeCList cl
             n @?= length ws]

  where
    vs :: [Int]
    vs = ws ++ ws
    ws = [1 .. 1000]

clistnfTests =
    testGroup
        "CList.NoFree"
        [ testCase "newCList" $
          do cl :: CLNF.CList Int <- CLNF.newCList
             n <- CLNF.readCList cl
             n @?= []
        , testCase "pushCList" $
          do cl <- CLNF.newCList
             forM_ vs $ CLNF.pushCList cl
             n <- CLNF.readCList cl
             n @?= vs
        , testCase "popCList" $
          do cl <- CLNF.newCList
             forM_ vs $ CLNF.pushCList cl
             forM_ vs . const $ CLNF.popCList cl
             n <- CLNF.readCList cl
             n @?= []
        , testCase "sizeCList" $
          do cl <- CLNF.newCList
             forM_ vs $ CLNF.pushCList cl
             forM_ vs . const $ CLNF.popCList cl
             n <- CLNF.sizeCList cl
             n @?= 0]

  where
    vs :: [Int]
    vs = ws ++ ws
    ws = [1 .. 1000]

main :: IO ()
main = defaultMain tests
