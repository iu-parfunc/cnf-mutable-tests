{-# LANGUAGE BangPatterns #-}

-- | A version of the (fake) API indexed by a session/region type parameter.

module Data.Compact.Indexed (
    Compact,
    getCompact,
    inCompact,
    isCompact,
    newCompact,
    newCompactNoShare,
    appendCompact,
    appendCompactNoShare,
    ) where

import           Control.DeepSeq
import qualified Data.Compact    as C

-- | The indexed compact type.
newtype Compact s a = Compact (C.Compact a)

appendCompact :: NFData a => Compact s b -> a -> IO (Compact s a)
appendCompact (Compact !c) !a = Compact <$> C.appendCompact c a

appendCompactNoShare :: NFData a => Compact s b -> a -> IO (Compact s a)
appendCompactNoShare (Compact !c) !a = Compact <$> C.appendCompactNoShare c a

getCompact :: Compact s a -> a
getCompact (Compact !c) = C.getCompact c

inCompact :: Compact s a -> b -> Bool
inCompact (Compact !c) !b = C.inCompact c b

isCompact :: a -> Bool
isCompact = C.isCompact

newCompact :: NFData a => Word -> a -> IO (Compact s a)
newCompact !w !a = Compact <$> C.newCompact w a

newCompactNoShare :: NFData a => Word -> a -> IO (Compact s a)
newCompactNoShare !w !a = Compact <$> C.newCompactNoShare w a
