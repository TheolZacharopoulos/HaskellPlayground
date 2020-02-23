module Lecture1 where

import Data.List
import Test.QuickCheck

sentence :: [Char]
sentence = "Sentences can go " ++ onAndOn

onAndOn :: [Char]
onAndOn  = "on and " ++ onAndOn

s = take 65 sentence

sentences = "Sentences can go" : map (++ " and on") sentences

----------------------------

-- Implement your map
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

-- Implement your take
myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake n (x:xs) = x : myTake (n-1) xs

----------------------------

threefold :: Integer -> Bool
threefold n = rem n 3 == 0

threefolds = filter threefold [0..]

-- Implement you filter
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs)
    | f x == True = x : myFilter f xs
    | otherwise   = myFilter f xs

----------------------------
-- Proposition

-- Natural numbers
nats = [0..]

query1 = all (\ n -> any (\ m -> n < m) nats) nats
query2 = any (\ n -> all (\ m -> n <= m) nats) nats

-- We can make this look a bit more natural as follows:
forall = flip all
exist = flip any

-- any (>3) [0..] <=> (flip any) [0..] (>3)

query1' = forall nats (\ n -> exist nats (\ m -> n < m))
query2' = exist nats (\ n -> forall nats (\ m -> n <= m))

-- Implement you all
myAll :: (a -> Bool) -> [a] -> Bool
myAll _ [] = True
myAll f (x:xs) = f x && myAll f xs

-----------------------------
-- Instead of a function we need a list.
-- Here is a general conversion from lists to properties:
list2p :: Eq a => [a] -> a -> Bool
list2p = flip elem

myallTest :: [Int] -> [Int] -> Bool
myallTest = \ ys xs -> let p = list2p ys in
  all p xs == myAll p xs

-- check: quickCheck myallTest


