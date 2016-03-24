-- | A fake version of the real Data.Compact library that exposes the same interface.

module Data.Compact where

import Control.DeepSeq

-- | The dummy compact type.
newtype Compact a = Compact a

appendCompact ::
  NFData a => Compact b -> a -> IO (Compact a)
appendCompact = undefined

appendCompactNoShare ::
  NFData a => Compact b -> a -> IO (Compact a)
appendCompactNoShare = undefined

getCompact :: Compact a -> a
getCompact = undefined

-- | This one is hard to fake....
inCompact :: Compact a -> b -> Bool
inCompact = error "inCompact: cannot implement in this dummy implementation"

-- | This one too
isCompact :: a -> Bool
isCompact = error "isCompact: cannot implement in this dummy implementation"

newCompact ::
  NFData a => Word -> a -> IO (Compact a)
newCompact = undefined

newCompactNoShare ::
  NFData a => Word -> a -> IO (Compact a)
newCompactNoShare = undefined
