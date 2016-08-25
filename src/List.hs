module List where

import Control.Monad (guard)

{-

Project Euler 9
===============

A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a^2 + b^2 = c^2
For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for
which a + b + c = 1000.
Find the product a*b*c.

-}

run :: IO ()
run = print triples

triples :: [Int]
triples = do
  a <- [1..1000]
  b <- [1..a]
  let c = 1000 - a - b
  guard (a*a + b*b == c*c)
  pure (a*b*c)
