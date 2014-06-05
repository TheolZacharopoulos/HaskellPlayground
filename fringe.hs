-- a function that returns a list of all elements in the
-- leafs of a tree from left to right

fringe :: Tree a -> [a]

fringe (Leaf x) = [x]
fringe (Branch left right) = fringe left ++ fringe right
