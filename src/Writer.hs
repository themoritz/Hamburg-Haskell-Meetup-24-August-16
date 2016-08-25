module Writer where

import Control.Monad.Writer

data Tree
  = Leaf Int
  | Node [Tree]

testTree :: Tree
testTree = Node
  [ Node
    [ Node
      [ Leaf 34
      , Leaf 3042
      ]
    , Node
      [ Leaf 324
      , Node
        [ Leaf 324
        ]
      ]
    , Leaf 321
    ]
  , Leaf 5
  ]

go :: Tree -> Writer [Int] ()
go tree = case tree of
  Leaf x -> if even x then tell [x] else tell []
  Node subs -> mapM_ go subs

collectEvens :: Tree -> [Int]
collectEvens tree = execWriter (go tree)

run :: IO ()
run = print $ sum $ collectEvens testTree
