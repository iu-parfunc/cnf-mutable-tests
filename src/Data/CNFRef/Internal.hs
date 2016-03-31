{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

-- |

module Data.CNFRef.Internal where

import Foreign
import GHC.Exts

unsafeSizeof :: a -> Word
unsafeSizeof a = fromIntegral $
  case unpackClosure# a of
    (# _, ptrs, nptrs #) ->
      sizeOf (undefined :: Int) + -- one word for the header
      I# (sizeofByteArray# (unsafeCoerce# ptrs)
          +# sizeofByteArray# nptrs)
