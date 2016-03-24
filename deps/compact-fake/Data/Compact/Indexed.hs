-- | A version of the (fake) API indexed by a session/region type parameter.

module Data.Compact.Indexed where

import Control.DeepSeq

-- | The dummy compact type.
newtype Compact s a = Compact a

appendCompact :: NFData a => Compact s b -> a -> IO (Compact s a)
appendCompact = undefined

appendCompactNoShare :: NFData a => Compact s b -> a -> IO (Compact s a)
appendCompactNoShare = undefined

getCompact :: Compact s a -> a
getCompact = undefined

-- | This one is hard to fake....
inCompact :: Compact s a -> b -> Bool
inCompact = error "inCompact: cannot implement in this dummy implementation"

-- | This one too
isCompact :: a -> Bool
isCompact = error "isCompact: cannot implement in this dummy implementation"

newCompact :: NFData a => Word -> a -> IO (Compact s a)
newCompact = undefined

newCompactNoShare :: NFData a => Word -> a -> IO (Compact s a)
newCompactNoShare = undefined
