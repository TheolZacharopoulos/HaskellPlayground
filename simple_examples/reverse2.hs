-- Use foldr for reverse definition.
reverse2 = foldr (\x xs -> xs ++ [x]) []
