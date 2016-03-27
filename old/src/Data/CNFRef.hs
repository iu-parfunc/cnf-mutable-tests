{-# LANGUAGE BangPatterns #-}

module Data.CNFRef
       ( CNFRef
       , In
       , copy
       , newCNFRef
       , readCNFRef
       , writeCNFRef
       , modifyCNFRef
       , atomicWriteCNFRef
       , atomicModifyCNFRef
       ) where

import Control.DeepSeq
import Data.CNFRef.DeepStrict
import Data.Compact
import Data.IORef

newtype CNFRef s a = CNFRef (Compact (IORef a))

newtype In s a = In (Compact a)

copy :: DeepStrict a => CNFRef s b -> a -> IO (In s a)
copy (CNFRef !c) !a = do c' <- appendCompact c a
                         return $ In c'

-- class Tracked t where
--   copy :: DeepStrict (t s1) => t s1 -> t s2

newCNFRef :: DeepStrict a => In s a -> IO (CNFRef s a)
newCNFRef (In !c) = do let a = getCompact c
                       r <- newIORef a
                       c' <- appendCompact c r
                       return $ CNFRef c'

readCNFRef :: DeepStrict a => CNFRef s a -> IO (In s a)
readCNFRef ref@(CNFRef !c) = do let io = getCompact c
                                a <- readIORef io
                                copy ref a

writeCNFRef :: CNFRef s a -> In s a -> IO ()
writeCNFRef (CNFRef !c) (In !c') = do let io = getCompact c
                                          a = getCompact c'
                                      writeIORef io a

modifyCNFRef :: DeepStrict a => CNFRef s a -> (In s a -> In s a) -> IO ()
modifyCNFRef !ref !f = do a <- readCNFRef ref
                          let b = f a
                          writeCNFRef ref b

atomicWriteCNFRef :: CNFRef s a -> In s a -> IO ()
atomicWriteCNFRef (CNFRef !c) (In !c') = do let io = getCompact c
                                                a = getCompact c'
                                            atomicWriteIORef io a

atomicModifyCNFRef :: DeepStrict a => CNFRef s a -> (In s a -> In s a) -> IO ()
atomicModifyCNFRef !ref !f = do a <- readCNFRef ref
                                let b = f a
                                atomicWriteCNFRef ref b
