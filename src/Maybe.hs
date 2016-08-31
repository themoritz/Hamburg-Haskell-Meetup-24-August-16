module Maybe where

import           Data.Map  as Map (Map, fromList, lookup)
import           Text.Read (readMaybe)

run :: IO ()
run = do
  putStrLn "Please enter a number:"
  input1 <- getLine
  putStrLn "Please enter an operator:"
  input2 <- getLine
  putStrLn "Please enter a number:"
  input3 <- getLine
  let mResult = getResult input1 input2 input3
  putStrLn $ case mResult of
    Just result -> show result
    Nothing     -> "Could not process input."
  putStrLn ""
  run

operators :: Map String (Integer -> Integer -> Integer)
operators = Map.fromList
  [ ("+", (+))
  , ("-", (-))
  , ("*", (*))
  , ("/", div)
  , ("%", mod)
  ]

getResult :: String -> String -> String -> Maybe Integer
getResult i1 i2 i3 = do
  a <- readMaybe i1
  c <- readMaybe i3
  o <- Map.lookup i2 operators
  pure (o a c)
