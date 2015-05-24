-- A triple (x, y, z) of positive integers is pythagorean if x2+y2=z2.
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]
