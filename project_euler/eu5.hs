module Eu5 where

import Data.List

-- 2520 is the smallest number that can be divided by 
-- each of the numbers from 1 to 10 without any remainder.
-- What is the smallest positive number that is evenly 
-- divisible by all of the numbers from 1 to 20?

-- Check for numbers with no reminders
noRemainder :: Integral a => a -> a-> Bool
noRemainder d n = mod d n == 0

noRemList len x = all (noRemainder x) [1..len]
getNoRemLists len = map (noRemList len)

-- Slow
eu5 = findIndex (==True) $ getNoRemLists 20 [2521..]
