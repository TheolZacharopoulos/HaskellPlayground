-- The function decides if a value is an element of a list

elemRec :: Eq a => a -> [a] -> Bool
elemRec _ [] = False
elemRec x (y:ys) 
    | x == y = True
    | otherwise = elem x ys
