module FlatMap where

flatMapM :: Maybe Int -> (Int -> Maybe Int) -> Maybe Int
flatMapM mx f = case mx of
  Nothing -> Nothing
  Just x -> f x

flatMapL :: [Int] -> (Int -> [Int]) -> [Int]
flatMapL xs f = case xs of
  [] -> []
  x:rest -> f x ++ flatMapL rest f

f1 :: Int -> Maybe Int
f1 x = if even x then Just (x `div` 2) else Nothing

f2 :: Int -> Maybe Int
f2 x = if even x then Just (x + 1) else Nothing

goM :: Maybe Int
goM = do
  x <- f1 12   -- flatMapM (f1 5) (\x ->
  y <- f2 x    --   flatMapM (f2 x) (\y ->
  pure (x + y) --     Just (x + y)
               --   )
               -- )

goM' :: Maybe Int
goM' =
  case f1 12 of
    Nothing -> Nothing
    Just x -> case f2 x of
      Nothing -> Nothing
      Just y -> Just (x + y)

duplicate :: a -> [a]
duplicate x = [x, x]

goL :: [Int]
goL = do
  x <- [1, 2]      -- flatMapL [1, 2] (\x ->
  y <- duplicate x --   flatMapL (duplicate x) (\y ->
  pure (x + y)     --     [x + y]
                   --   )
                   -- )

run :: IO ()
run = do
  print goM
  print goL