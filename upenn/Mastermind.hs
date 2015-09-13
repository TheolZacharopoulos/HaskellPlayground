{-# OPTIONS_GHC -Wall #-}
module Mastermind where

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches xs ys = length $ filter (==True) $ zipWith (==) xs ys

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors xs = map (\y -> length $ filter (\e -> e == y) xs) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches xs ys = sum $ zipWith (min) (countColors xs) (countColors ys)

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess =
	let exactMatchesNum = exactMatches secret guess in 
		Move guess exactMatchesNum $ matches secret guess - exactMatchesNum

-- Checks if a given Move is concistent to the secret Code.
isConsistent :: Move -> Code -> Bool
isConsistent move@(Move guess _ _) secret = (getMove secret guess) == move

-- Filter a List of Codes to contain those that are consistent with a given Move.
filterCodes :: Move -> [Code] -> [Code]
filterCodes move guesses = filter (isConsistent move) guesses

-- Choose k elements from n elements equals choose k - 1 elements from n - 1 elements plus choose k elements from n - 1 elements.
combinations :: Int -> [a] -> [[a]]
combinations _ [] = [[]]
combinations k xs = combinations' (length xs) k xs
  where
  	combinations' _ _ [] = [[]]
  	combinations' n k' arr@(y:ys)
          | k' == 0   = [[]]
          | k' >= n   = [arr]
          | null arr  = []
          | otherwise = map (y :) (combinations' (n - 1) (k' - 1) ys) ++ combinations' (n - 1) k' ys 

-- Generates all possible color codes. 
allCodes :: Int -> [Code]
allCodes len = combinations len colors -- less code: flip combinations colors

-- The solver function takes in a secret Code and outputs a list of Moves that the computer used as clues to figure out the secret.
-- our game solver will start with a list of all possible codes and gradually filter the list based on each new move until there is only one code left.
solve :: Code -> [Move]
solve secret = (codeMoves secret (allCodes 4))
	where 
		codeMoves :: Code -> [Code] -> [Move]
		codeMoves _ [] = []
		codeMoves secret' guesses = map (getMove secret') guesses

		movesOfCodes :: [Code] -> [Move]
		movesOfCodes [] = []
		movesOfCodes (code:codes) = 
			let move = (getMove secret code) in
		 		move : movesOfCodes (filterCodes move codes)

-- Knuth's five guess algorithm
fiveGuess :: Code -> [Move]
fiveGuess = undefined