{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE TypeFamilies        #-}

module Main where

import Control.Monad
import Test.Tasty
import Test.Tasty.HUnit

import           Data.Compact
import           Data.IORef
import           Data.Primitive.MutVar
import qualified Data.Vector                 as B
import qualified Data.Vector.Mutable         as V
import qualified Data.Vector.Unboxed.Mutable as U

import Data.IntBox as IB

import qualified Data.CList        as CL
import           Data.CList.MList  as ML
import qualified Data.CList.NoFree as CLNF

import qualified Data.ChanBox.V0 as CB0
import qualified Data.ChanBox.V1 as CB1
import qualified Data.ChanBox.V2 as CB2

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

printU :: (U.Unbox a, Show a) => U.IOVector a -> IO ()
printU a = go 0 (U.length a)
  where go i l =
          if i < l
             then do x <- U.read a i
                     print x
                     go (i + 1) l
             else return ()

readU :: U.Unbox a => U.IOVector a -> IO [a]
readU a = go 0 (U.length a)
  where go i l =
          if i < l
             then do x <- U.read a i
                     xs <- go (i + 1) l
                     return (x : xs)
             else return []

printB :: Show a => B.Vector a -> IO ()
printB a = go 0 (B.length a)
  where go i l =
          if i < l
             then do let x = a B.! i
                     print x
                     go (i + 1) l
             else return ()

readB :: B.Vector a -> IO [a]
readB a = go 0 (B.length a)
  where go i l =
          if i < l
             then do let x = a B.! i
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
        , iovectorTests
        , boxedvectorTests
        , boxediovectorTests
        , boxeduiovectorTests
        , intboxTests
        -- , mlistTests
        , clistTests
        , clistnfTests
        , chanboxv1Tests
        -- broken
        -- , chanboxv2Tests
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
             forM_ vs $ \v -> writeIntBox ib v
             n <- readIntBox ib
             n @?= vs]

boxedvectorTests =
    testGroup
        "Boxed immutable Vector in a compact"
        [ testCase "B.Vector Int in a compact" $
          do let x :: B.Vector Int = B.replicate 5 42
             c <- newCompact 64 x
             let y :: B.Vector Int = B.replicate 5 21
             c' <- appendCompact c y
             z :: [Int] <- readB (getCompact c')
             z @?= replicate 5 21]

boxediovectorTests =
    testGroup
        "Boxed immutable Vector in a compact"
        [ testCase "B.Vector (U.IOVector Int) in a compact" $
          do v :: U.IOVector Int <- U.replicate 5 42
             let x :: B.Vector (U.IOVector Int) = B.replicate 5 v
             c <- newCompact 64 x
             v' :: U.IOVector Int <- U.replicate 5 21
             let y :: B.Vector (U.IOVector Int) = B.replicate 5 v'
             c' <- appendCompact c y
             z :: [U.IOVector Int] <- readB (getCompact c')
             not (null z) @? "not empty" ]

boxeduiovectorTests =
    testGroup
        "Boxed immutable Vector in a compact"
        [ testCase "B.Vector (V.IOVector Int) in a compact" $
          do v :: V.IOVector Int <- V.replicate 5 42
             let x :: B.Vector (V.IOVector Int) = B.replicate 5 v
             c <- newCompact 64 x
             v' :: V.IOVector Int <- V.replicate 5 21
             let y :: B.Vector (V.IOVector Int) = B.replicate 5 v'
             c' <- appendCompact c y
             z :: [V.IOVector Int] <- readB (getCompact c')
             not (null z) @? "not empty" ]

-- mlistTests =
--     testGroup
--         "MList"
--         [ testCase "newMList" $
--           do m <- newCNFRef (Nil :: List Int)
--              n <- readMList m
--              n @?= []
--         , testCase "updateMList" $
--           do m <- newCNFRef (Nil :: List Int)
--              forM_ vs $ (updateMList m =<<) . copyToCompact m
--              n <- readMList m
--              n @?= ws
--         , testCase "appendMList" $
--           do m <- newCNFRef (Nil :: List Int)
--              forM_ vs $
--                  \a ->
--                       do vec <- newVec a
--                          ref <- newIORef Nil
--                          c <- copyToCompact m $ Cons vec ref
--                          appendMList m c
--              n <- readMList m
--              n @?= vs
--         , testCase "dropMList" $
--           do m <- newCNFRef (Nil :: List Int)
--              forM_ vs $ (updateMList m =<<) . copyToCompact m
--              forM_ ws $ dropMList m
--              n <- readMList m
--              n @?= []
--         , testCase "popMList" $
--           do m <- newCNFRef (Nil :: List Int)
--              forM_ vs $ (updateMList m =<<) . copyToCompact m
--              forM_ ws . const $ popMList m
--              n <- readMList m
--              n @?= []
--         , testCase "lengthMList" $
--           do m <- newCNFRef (Nil :: List Int)
--              forM_ vs $ (updateMList m =<<) . copyToCompact m
--              n <- lengthMList m
--              n @?= length ws]
--   where
--     vs :: [Int]
--     vs = ws ++ ws
--     ws = [1 .. 1000]

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
    ws = [1 .. 100]

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
    ws = [1 .. 100]

chanboxv0Tests =
    testGroup
        "ChanBox.V0"
        [ testCase "newBox" $
          do cb <- CB0.newBox
             sz <- CB0.sizeBox cb
             sz @?= 0
        , testCase "pushMsg" $
          do cb <- CB0.newBox
             msgs <- forM vs $ \i -> CB0.newMessage cb i
             forM msgs $ CB0.pushMsg cb
             sz <- CB0.sizeBox cb
             sz @?= 2 * l]

  where
    l :: Int
    l = 200
    vs :: [Int]
    vs = ws ++ ws
    ws = [1 .. l]

chanboxv1Tests =
    testGroup
        "ChanBox.V1"
        [ testCase "newBox" $
          do cb <- CB1.newBox' max
             sz <- CB1.sizeBox cb
             sz @?= 0
        , testCase "pushMsg" $
          do cb <- CB1.newBox' max
             msgs <- forM vs $ \i -> CB1.newMessage cb i
             forM msgs $ CB1.pushMsg cb
             sz <- CB1.sizeBox cb
             sz @?= min (2 * l) max ]

  where
    max :: Int
    max = 60
    l :: Int
    l = 200
    vs :: [Int]
    vs = ws ++ ws
    ws = [1 .. l]

chanboxv2Tests =
    testGroup
        "ChanBox.V2"
        [ testCase "newBox" $
          do cb <- CB2.newBox
             sz <- CB2.sizeBox cb
             sz @?= 0
        , testCase "pushMsg" $
          do cb <- CB2.newBox
             msgs <- forM vs $ \i -> CB2.newMessage cb i
             forM msgs $ CB2.pushMsg cb
             sz <- CB2.sizeBox cb
             sz @?= 2 * l]

  where
    l :: Int
    l = 5
    vs :: [Int]
    vs = ws ++ ws
    ws = [1 .. l]

main :: IO ()
main = defaultMain tests
