module List where

{-

Project Euler 9
===============

A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a^2 + b^2 = c^2
For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product a*b*c.

-}

run :: IO ()
run = print $ (\(a,b,c) -> a*b*c) special

max' :: Int
max' = 1000

-- list :: [Int]
list = [1..max']

special :: (Int, Int, Int)
special = head $ filter (\(a,b,c) -> a*a+b*b == c*c && a+b+c == max') triples

triples :: [(Int, Int, Int)]
triples = do
  x <- pairs
  triple x

triple :: (Int, Int) -> [(Int, Int, Int)]
triple a =
  [(fst(a), snd(a), max'-fst(a)-snd(a))]

pairs :: [(Int, Int)]
pairs = do
  x <- list
  pair x

pair :: Int -> [(Int, Int)]
pair a =
  fmap (\x -> (a, x)) list
