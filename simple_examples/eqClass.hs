class Eq a where
    (==)    ::  a -> a -> Bool

-- (==) ::  (Eq a) => a -> a -> Bool
-- elem :: (Eq a ) => a -> [a] -> Bool

instance Eq Integer where 
    x == y  =  x `integerEq` y

instance Eq Float where
    x == y  =  x `floatEq` y

-- From prelude
class Eq a where
    (==), (/=)  :: a -> a -> Bool
    x /= y  =  not (x==y)

-- We may wish to define a class Ord which inherits all of the operations in Eq, but in addition has a set of comparison operations and minimum and maximum functions:
-- Here We say that Eq is a superclass of Ord.
class (Eq a) => Ord a where
    (<), (<=), (>=), (>)    :: a -> a -> Bool
    max, min                :: a -> a -> a

