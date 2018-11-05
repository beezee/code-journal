module TailrecTT where

import Data.List

data Tree x = Branch x (Tree x) (Tree x) | Leaf x | TNil deriving Show

cata :: Tree x -> [Tree x] -> [x] -> [x]
cata (Branch x l r) defer acc = cata l (r: (Leaf x): defer) acc
cata (Leaf x) (d:defer) acc = cata d defer (acc ++ [x])
cata (Leaf x) [] acc = (acc ++ [x])
cata TNil (d:defer) acc = cata d defer acc
cata TNil [] acc = acc

run = (Branch 42
  (Branch 7
    (Branch 8 (Leaf 17) (Leaf 9))
    (Branch 9 (Leaf 11) TNil))
  (Branch 3
    (Branch 5 (Leaf 7) (Leaf 8))
    (Branch 10 (Leaf 11) TNil)))
