
-- | A fake version of the real Data.Compact library that exposes the same interface.

module Data.Compact where

import Control.DeepSeq

-- | The dummy compact type.
newtype Compact a = Compact a

appendCompact :: NFData a => Compact b -> a -> IO (Compact a)
appendCompact _ x = return (Compact x)

appendCompactNoShare :: NFData a => Compact b -> a -> IO (Compact a)
appendCompactNoShare _ x = return (Compact x)

getCompact :: Compact a -> a
getCompact (Compact a) = a

-- | This one is hard to fake....
inCompact :: Compact a -> b -> IO Bool
inCompact = error "inCompact: cannot implement in this dummy implementation"

-- | This one too:
--
-- We need to be able to enumarate all the reachable heap objects for
-- arbitrary things in NFData...  that would be a feat of generic programming.
isCompact :: a -> IO Bool
isCompact = error "isCompact: cannot implement in this dummy implementation"

newCompact :: NFData a => Word -> a -> IO (Compact a)
newCompact _ x = return (Compact x)

newCompactNoShare :: NFData a => Word -> a -> IO (Compact a)
newCompactNoShare _ x = return (Compact x)
