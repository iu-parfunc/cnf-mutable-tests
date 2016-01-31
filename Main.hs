{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import           Control.DeepSeq
import           Data.Compact
import           Data.IORef
import qualified Data.Vector.Mutable as V
import           System.IO.Unsafe

instance NFData a => NFData (IORef a) where
  rnf a = unsafePerformIO $ modifyIORef' a force

instance NFData a => NFData (V.IOVector a) where
  rnf a = unsafePerformIO $ modifyIOVector' a force
    where modifyIOVector' !a !f = go' a f 0 (V.length a)
          go' !a !f !i !l = if i < l
                               then V.unsafeModify a f i >>= \ !_ -> go' a f (i + 1) l
                               else return ()

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
test3 = do x :: V.IOVector Int <- V.new 5
           _ <- V.set x 42
           c <- newCompact 64 x
           y <- V.new 5
           _ <- V.set y 21
           c' <- appendCompact c y
           z :: Int <- V.read (getCompact c') 0
           print z

main :: IO ()
main = test1 >> test2 >> test3
