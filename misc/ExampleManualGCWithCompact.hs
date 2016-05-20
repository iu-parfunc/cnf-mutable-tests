{-# LANGUAGE DeriveGeneric #-}

module Main
( main
)
where

import Prelude hiding (Left, Right)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.IORef
import Data.Time
import Data.Compact
import Control.Monad
import Control.Exception (finally)
import Control.DeepSeq
import Control.Concurrent
import System.Random
import GHC.Generics

-- | Binary tree
data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving (Eq, Show, Generic)

-- | Path in the tree
data Path
  = Left
  | Right
  deriving (Eq, Show)

randomPath :: IO Path
randomPath = do
  b <- randomIO
  case b of
    True -> return Left
    False -> return Right

-- | Find a value in tree at the path
find :: [Path] -> Tree a -> Maybe a
find [] (Leaf a) = Just a
find (Left : path) (Node l _) = find path l
find (Right : path) (Node _ r) = find path r
find _ _ = Nothing

-- | Set value at path
set :: [Path] -> a -> Tree a -> Tree a
set [] a _ = Leaf a
set (Left : path) a (Node l r) =
  let l' = set path a l
  in l' `seq` Node l' r
set (Right : path) a (Node l r) =
  let r' = set path a r
  in r' `seq` Node l r'

instance NFData a => NFData (Tree a)

-- | Depth of generated test tree
maxDepth :: Int
maxDepth = 20

-- | Depth of nodes evacuated during GC
evacDepth :: Int
evacDepth = 10

-- | Depth of nodes compact during GC
compactDepth :: Int
compactDepth = 10

main :: IO ()
main = do
  -- generate test tree
  tree <- generate maxDepth :: IO (Tree Int)
  var <- newMVar tree

  void $ forkIO (gcLoop var)

  void $ forkIO (mutateLoop var)

  threadDelay 1000000
  histRef <- newIORef $ Map.fromList $ zip [0..100] (repeat 0)
  readLoop var histRef
    `finally` writeHist histRef

writeHist :: IORef (Map Int Int) -> IO ()
writeHist ref = readIORef ref >>= print . filter (\(_, v) -> v /= 0) . Map.toList

generate :: Random a => Int -> IO (Tree a)
generate 0 = do
  v <- randomIO
  return (Leaf v)
generate n = do
  l <- generate (pred n)
  r <- generate (pred n)
  return (Node l r)

gcLoop :: MVar (Tree Int) -> IO ()
gcLoop =
  -- two algorithms, just compact and evacuate
  if False
    then gcLoopCompact
    else gcLoopEvacuate

-- | Compacting: create a compact region and append tree nodes to it
gcLoopCompact :: MVar (Tree Int) -> IO ()
gcLoopCompact var = do
  comp <- newCompact 10000 ()
  forever $ do
    threadDelay 100000
    modifyMVar_ var $ \tree -> do
      compactTree comp compactDepth tree

compactTree :: Compact a -> Int -> Tree Int -> IO (Tree Int)
compactTree comp 0 t = do
  ok <- inCompact comp t
  if ok
    then return t
    else getCompact <$> appendCompact comp t
compactTree comp n (Node l r) = do
  l' <- do
    ok <- inCompact comp l
    if ok
      then return l
      else compactTree comp (pred n) l
  r' <- do
    ok <- inCompact comp r
    if ok
      then return r
      else compactTree comp (pred n) r
  return (Node l' r')
compactTree comp _ t = do
  ok <- inCompact comp t
  if ok
    then return t
    else getCompact <$> appendCompact comp t

-- | Evaluate: each time create a fresh compact and copy tree nodes to it
gcLoopEvacuate :: MVar (Tree Int) -> IO ()
gcLoopEvacuate var = do
  -- initial compaction, blocks everything
  modifyMVar_ var $ \tree -> do
    comp <- newCompact 10000 tree
    let loop t = do
          (t', done) <- evacuateTree comp evacDepth t
          if done
            then return t'
            else loop t'
    loop tree

  forever $ do
    threadDelay 10000000
    comp <- newCompact 10000 ()
    loop comp
  where
  loop comp = do
    yield
    done <- modifyMVar var (evacuateTree comp evacDepth)
    unless done
      (loop comp)

evacuateTree :: Compact a -> Int -> Tree Int -> IO (Tree Int, Bool)
evacuateTree comp 0 tree = do
  ok <- inCompact comp tree
  if ok
    then return (tree, True)
    else do
      tree' <- getCompact <$> appendCompact comp tree
      return (tree', False)
evacuateTree comp n (Node l r) = do
  (l', doneL) <- evacuateTree comp (pred n) l
  if doneL
    then do
      (r', doneR) <- evacuateTree comp (pred n) r
      return (Node l' r', doneR)
    else return (Node l' r, doneL)
evacuateTree _ _ t = return (t, True)

-- | Read values at random path and measure latency
readLoop :: MVar (Tree Int) -> IORef (Map Int Int) -> IO ()
readLoop var histRef = forever $ do
  threadDelay 100
  t1 <- getCurrentTime
  replicateM_ 1000 $ do
    path <- replicateM maxDepth randomPath
    tree <- readMVar var
    case find path tree of
      Nothing -> error "Nothing!"
      Just _ -> return ()
  t2 <- getCurrentTime
  let diff = diffUTCTime t2 t1
      k = floor (diff * 1000) :: Int
  modifyIORef' histRef $!
    Map.update (\v -> Just $! (succ v)) k
  print (diffUTCTime t2 t1)

-- | Update tree at random paths
mutateLoop :: MVar (Tree Int) -> IO ()
mutateLoop var = forever $ do
  threadDelay 100
  path <- replicateM maxDepth randomPath
  v <- randomIO
  modifyMVar_ var $ \tree -> do
    let tree' = set path v tree
    tree' `seq` return tree'