{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE StrictData                #-}

-- | A linked-list like data structure in a compact

module Data.CList (
    CList,
    newCList,
    readCList,
    writeCList,
    popCList,
    sizeCList,
    ) where

import Control.Monad
import Data.CList.MList
import Data.CNFRef
import Data.CNFRef.DeepStrict
import Data.Compact.Indexed
import Data.IORef
import Data.Vector.Unboxed.Mutable

-- | A compact list
data CList a = forall s. CList { rootList :: MList s a -- ^ root
                               , freeList :: MList s a -- ^ free
                               }

newCList :: DeepStrict a => IO (CList a)
newCList = do
  root <- newCNFRef Nil
  free <- newCNFRef Nil
  -- FIXME:
  -- free <- appendCNFRef root Nil
  return $ CList root free

readCList :: (DeepStrict a, Unbox a) => CList a -> IO [a]
readCList CList { .. } = readMList rootList

writeCList :: (DeepStrict a, Unbox a, Eq a) => CList a -> a -> IO ()
writeCList CList { .. } a = do
  c <- dropMList freeList a
  case getCompact c of
    Nil -> do
      vec <- newVec a
      ref <- newIORef Nil
      c' <- copyToCompact rootList $ Cons vec ref
      writeMList rootList c'
    Cons _ _ ->
      writeMList rootList c

popCList :: (DeepStrict a, Unbox a, Eq a) => CList a -> IO (Maybe a)
popCList CList { .. } = do
  v <- popMList rootList
  case v of
    Just a  -> updateMList freeList a
    Nothing -> return ()
  return v

sizeCList :: (DeepStrict a, Unbox a) => CList a -> IO Int
sizeCList CList { .. } =
  liftM2 (+) (lengthMList rootList) (lengthMList freeList)
