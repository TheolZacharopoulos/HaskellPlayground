-- length function takes as arg a List and returns an integer that represents the length of that list
myLength :: [a] -> Integer

-- "Pattern Matching" when given empty list the length is 0
myLength [] = 0

-- The length of a list whose first element is x and the rest of the list
-- is xs, is 1 plus the length of xs. 
myLength (x:xs) = 1 + myLength xs 
