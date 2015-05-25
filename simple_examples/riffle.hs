--Takes two lists of the same length and interleaves their elements in turn about order.

riffle :: [a] -> [a] -> [a]
riffle xs ys = concat [[x, y] | (x, y) <- xs `zip` ys]
