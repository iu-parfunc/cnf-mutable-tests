module Data.CNFRef where

import Data.IORef

newtype CNFRef s a = CNFRef (IORef a)
