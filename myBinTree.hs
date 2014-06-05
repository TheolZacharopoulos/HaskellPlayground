-- Recursive type : Binary tree

-- The elements are either leaf nodes containing a value
-- of type a, or internal nodes "branches" containing recursively
-- two sub-trees.
data Tree a = Leaf a | Branch (Tree a) (Tree a)
