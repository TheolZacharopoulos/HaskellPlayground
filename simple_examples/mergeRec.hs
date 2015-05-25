-- merges two sorted lists in ascending order to give a single sorted list in ascending order.

mergeRec :: Ord a => [a] -> [a] -> [a]
mergeRec [] ys = ys
mergeRec xs [] = xs
mergeRec (x:xs) (y:ys)
    = if x <= y then x : mergeRec xs (y:ys) else y : mergeRec (x:xs) ys

