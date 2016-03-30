{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import           Control.DeepSeq
import           Data.Compact
import           Data.IORef
import           Data.Primitive.MutVar
import qualified Data.Vector.Mutable         as V
import qualified Data.Vector.Unboxed.Mutable as U
import           GHC.Prim
import           System.IO.Unsafe
import           System.Mem

import qualified ExampleDataStruct as IB

-- This is debatable... 
-- instance NFData a => NFData (IORef a) where
--   rnf a = unsafePerformIO $ modifyIORef' a force

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

test1 :: IO ()
test1 = do c <- newCompact 64 (42 :: Int)
           c' <- appendCompact c (21 :: Int)
           let x = getCompact c'
           print x

test2 :: IO ()
test2 = do x <- newIORef (42 :: Int)
           c <- newCompact 64 x
           y <- newIORef (21 :: Int)
           c' <- appendCompact c y
           z <- readIORef $ getCompact c'
           print z

test3 :: IO ()
test3 = do x <- newMutVar (42 :: Int)
           c <- newCompact 64 x
           y <- newMutVar (21 :: Int)
           c' <- appendCompact c y
           z <- readMutVar $ getCompact c'
           print z

test4 :: IO ()
test4 = do x :: U.IOVector Int <- U.new 5
           _ <- U.set x 42
           c <- newCompact 64 x
           y <- U.new 5
           _ <- U.set y 21
           c' <- appendCompact c y
           z :: [Int] <- readU (getCompact c')
           print z

test5 :: IO ()
test5 = do x :: V.IOVector Int <- V.new 5
           _ <- V.set x 42
           _ <- printV x
           _ <- performMajorGC
           c <- newCompact 64 x
           y <- V.new 5
           _ <- V.set y 21
           c' <- appendCompact c y
           z :: [Int] <- readV (getCompact c')
           print z


test6 :: IO ()
test6 = do ib <- IB.newIntBox
           IB.writeIntBox ib 33
           IB.writeIntBox ib 44
           n <- IB.readIntBox ib
           -- Assert n==44
           putStrLn "test6:"
           print n
           putStrLn "test6 complete"

-- FIXME: use tasty / HUnit.
main :: IO ()
main =
  do
     -- test1; test2; test3; test4; test5

     test6
