module BinLeafTree where

import Data.List
import Data.Char

-- Binary Leaf Tree.
data Blt a = Leaf a | Node (Blt a) (Blt a) deriving (Eq, Show)

exampleTree :: Blt String
exampleTree = Node (Node (Leaf "Hoare, Tony")
                         (Leaf "Turing, Alan"))
                   (Leaf "Goedel, Kurt")

-- Counts the number of leaf nodes in a binary tree.
leafCount ::  Blt a -> Int
leafCount (Leaf _) = 1
leafCount (Node left right) = leafCount left + leafCount right

-- Does for binary trees what map does for lists.
mapB :: (a -> b) -> Blt a -> Blt b
mapB f (Leaf x) = Leaf (f x)
mapB f (Node left right) = Node (mapB f left) (mapB f right)

-- Tree of arbitrary number of branches.
data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

example1 = T 1 [T 2 [], T 3 []]
example2 = T 0 [example1, example1, example1]

-- Counts the number of nodes of a tree.
count :: Tree a -> Int
count (T _ trees) = 1 + sum (map count trees)

-- Gives the depth of a tree
depth :: Tree a -> Int 
depth (T _ ts) = foldl max 0 (map depth ts) + 1

-- Does for trees what map does for lists.
mapT :: (a -> b) -> Tree a -> Tree b
mapT f (T node trees) = T (f node) (map (mapT f) trees)

-- Collects the information in a tree of type Tree a in a list of type [a].
collect :: Tree a -> [a]
collect (T node trees) = node : concat (map collect trees)

-- A fold operation on Trees
foldT :: (a -> [b] -> b) -> Tree a -> b
foldT f (T node ts) = f node (map (foldT f) ts)

-- Redefinition of the above using the foldT
count' = foldT (\ _ ts -> sum ts + 1)
depth' = foldT (\ _ ds -> if null ds then 0 else maximum ds + 1)
collect' = foldT (\ x lists ->  x : concat lists)
mapT' f = foldT (\ x ts ->  T (f x) ts)
