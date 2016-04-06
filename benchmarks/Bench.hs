module Main where

import Criterion.Main
import Data.CList

clistBench :: IO ()
clistBench = error "not yet implemented"

main :: IO ()
main = defaultMain [bench "CList" $ nfIO clistBench]
