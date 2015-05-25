-- Filter using foldr
filter2 f = foldr (\x xs -> if f x then x  : xs else xs) []
