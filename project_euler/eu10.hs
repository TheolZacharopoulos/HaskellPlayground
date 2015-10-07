-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
-- Find the sum of all the primes below two million.

-- https://en.wikipedia.org/wiki/Prime_number#Trial_division
factors :: Integer -> [Integer]
factors n = let 
   ps = takeWhile (\m -> m^2 <= n) primes
	 in factors' n ps where 
	   factors' 1 _  = []
	   factors' n [] = [n]
	   factors' n (p:ps) 
	    | n `mod` p == 0 = p: factors' (n `div` p) (p:ps)
	    | otherwise      =    factors' n ps

isPrime n = factors n == [n]
primes = 2 : filter isPrime [3..]

eu10 = sum $ takeWhile (<2000000) primes