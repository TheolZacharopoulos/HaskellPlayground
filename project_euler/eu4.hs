module ProjectEuler4 where

import Data.List

-- Convert a number to a list
numToList :: Integer -> [Integer]
numToList = map (\x -> read [x] :: Integer) . show

isPalidrome :: Integer -> Bool
isPalidrome n = reverse num == num
	where num = numToList n

maxPalidrome :: [Integer] -> [Integer] -> Integer
maxPalidrome xs ys = maximum $ filter isPalidrome $ generateProducts xs ys
	where
		generateProducts :: [Integer] -> [Integer] -> [Integer]
		generateProducts _ [] = []
		generateProducts xs (y:ys) = map (*y) xs ++ generateProducts xs ys 

eu4 = maxPalidrome [100..999] [100..999]