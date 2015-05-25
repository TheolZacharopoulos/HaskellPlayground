--  The function produces a list with n identical elements, Recursively.
replicateRec :: Int -> a -> [a]
replicateRec 0 _ = []
replicateRec n x = x : replicateRec (n-1) x
