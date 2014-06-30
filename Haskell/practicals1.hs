-- Functions 1

even :: Int -> Bool
even x = (x `mod` 2) == 0

-- Functions 2

area :: Double -> Double
area r = π * r * r where π = 22 / 7

-- Functions 3.a
smaller :: Int -> Int -> Int
smaller x y = if x <= y then x else y

-- Functions 3.c
st3 = smaller 3

-- Functions 4.a
square :: Int -> Int
square x = x * x

quad :: Int -> Int
quad = square . square

-- Functions 4.b
twice :: (a -> a) -> (a -> a)
twice f x = f (f x) -- it applies the same function twice

-- Functions 4.c
tquad :: Int -> Int
tquad =  twice square

-- Functions 5
ctwice :: (a -> a) -> (a -> a)
ctwice f = f . f

-- Functions 5.a: Yes, it behaves the same.
-- Functions 5.b: It compose two Functions

-- Functions 6
-- 6.1 Incorrect
-- 6.2 Correct
-- 6.3 Correct
-- 6.4 Incorrect
-- 6.5 Incorrect