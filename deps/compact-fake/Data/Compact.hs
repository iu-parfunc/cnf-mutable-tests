{-# LANGUAGE PackageImports #-}

-- | A fake version of the real Data.Compact library that exposes the same interface.

-- Re-export the ghc "compact" library.
module Data.Compact (module C) where

import "compact" Data.Compact as C
