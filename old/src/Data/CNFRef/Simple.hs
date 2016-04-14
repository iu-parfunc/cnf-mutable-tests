{-# LANGUAGE Strict     #-}
{-# LANGUAGE StrictData #-}

-- | An UNINDEXED version of the CNFRef...

module Data.CNFRef.Simple
       ( CNFRef
       , newCNFRef
       , readCNFRef
       , writeCNFRef
       , modifyCNFRef
       , atomicWriteCNFRef
       , atomicModifyCNFRef
       , readForCAS
       , casCNFRef
       ) where

import           Control.DeepSeq
import qualified Data.Atomics           as A
import           Data.CNFRef.DeepStrict
import           Data.Compact
import           Data.IORef
import           System.IO.Unsafe

newtype CNFRef a = CNFRef (Compact (IORef a))

instance Eq (CNFRef a) where
  (CNFRef c) == (CNFRef c') =
    let io = getCompact c
        io' = getCompact c'
    in inCompact c io' && inCompact c' io && io == io'

newCNFRef :: DeepStrict a => a -> IO (CNFRef a)
newCNFRef a = do r <- newIORef a
                 c <- newCompact 64 r
                 return $ CNFRef c

readCNFRef :: CNFRef a -> IO a
readCNFRef (CNFRef c) = readIORef $ getCompact c

writeCNFRef :: CNFRef a -> a -> IO ()
writeCNFRef (CNFRef c) a = writeIORef (getCompact c) a

modifyCNFRef :: CNFRef a -> (a -> a) -> IO ()
modifyCNFRef (CNFRef c) f = modifyIORef' (getCompact c) f

atomicWriteCNFRef :: CNFRef a -> a -> IO ()
atomicWriteCNFRef (CNFRef c) a = atomicWriteIORef (getCompact c) a

atomicModifyCNFRef :: CNFRef a -> (a -> (a , b)) -> IO b
atomicModifyCNFRef (CNFRef c) f = atomicModifyIORef' (getCompact c) f

readForCAS :: CNFRef a -> IO (A.Ticket a)
readForCAS (CNFRef c) = A.readForCAS $ getCompact c

casCNFRef :: CNFRef a -> A.Ticket a -> a -> IO (Bool , A.Ticket a)
casCNFRef (CNFRef c) t a = A.casIORef (getCompact c) t a
