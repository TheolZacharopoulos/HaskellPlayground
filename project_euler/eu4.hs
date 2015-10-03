module ProjectEuler4 where

-- A palindromic number reads the same both ways. 
-- The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
-- Find the largest palindrome made from the product of two 3-digit numbers.

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
