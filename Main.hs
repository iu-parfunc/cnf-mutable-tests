module Main where

import Control.DeepSeq
import Data.Compact
import Data.IORef
-- import Data.Vector.Mutable
import System.IO.Unsafe

-- instance NFData (IORef a) where
--   rnf a = a `seq` ()

instance NFData a => NFData (IORef a) where
  rnf a = unsafePerformIO $ modifyIORef a force

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

-- test3 :: IO ()
-- test3 = do a <- new 5
--            c <- newCompact 64 a
--            let a' = getCompact c
--            print a'

main :: IO ()
main = test2
