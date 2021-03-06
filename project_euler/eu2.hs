-- Each new term in the Fibonacci sequence is generated by adding the previous two terms
--  By starting with 1 and 2, the first 10 terms will be:
--
-- 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
--
-- By considering the terms in the Fibonacci sequence whose values do not exceed 
-- four million, find the sum of the even-valued terms.


-- Fibonacci
fib = map fst (iterate f (1, 2)) 
    where f (x,y) = (y, x + y)

eu2 = sum [x | x <- takeWhile ( < 4000000) fib, even x]
