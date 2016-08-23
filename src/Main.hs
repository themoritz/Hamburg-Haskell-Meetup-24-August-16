module Main where

import qualified Maybe
import qualified List
import qualified State
import qualified Writer

main :: IO ()
main = do
  Maybe.run
  List.run
  Writer.run
  State.run