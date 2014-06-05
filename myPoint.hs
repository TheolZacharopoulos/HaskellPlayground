-- Data construction
data Point = Pt a a

-- Type construction
Pt 2.0 3.0 :: Point Float
Pt 'a' 'b' :: Point Char
Pt True False :: Point Bool

