module State where

import Control.Monad.State
import Data.Ord
import Data.List
import Data.IntMap as IntMap

{-

Project Euler 14
================

The following iterative sequence is defined for the set of positive integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

It can be seen that this sequence (starting at 13 and finishing at 1) contains
10 terms. Although it has not been proved yet (Collatz Problem), it is thought
that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

-}

step :: Int -> Int
step x = if even x
           then x `div` 2
           else 3 * x + 1

chainLength :: Int -> State (IntMap Int) Int
chainLength x = do
  cache <- get
  case IntMap.lookup x cache of
    Just l -> pure l
    Nothing -> do
      stepChainLength <- chainLength (step x)
      let xChainLength = stepChainLength + 1
      put (IntMap.insert x xChainLength cache)
      pure xChainLength

chainLengths :: Int -> State (IntMap Int) [Int]
chainLengths x = mapM chainLength [1..x]

longest :: Int -> (Int, Int)
longest x =
  maximumBy (comparing snd) $
  zip [1..x] $
  evalState (chainLengths x) (IntMap.singleton 1 1)

run :: IO ()
run = print $ longest 999999
