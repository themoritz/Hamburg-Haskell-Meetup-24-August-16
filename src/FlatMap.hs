module FlatMap where

-- flatMapM ::

-- flatMapL ::

f1 :: Int -> Maybe Int
f1 x = if even x then Just (x `div` 2) else Nothing

f2 :: Int -> Maybe Int
f2 x = if even x then Just (x + 1) else Nothing

goM :: Maybe Int
goM = undefined

duplicate :: a -> [a]
duplicate x = [x, x]

goL :: [Int]
goL = undefined

run :: IO ()
run = do
  print goM
  print goL