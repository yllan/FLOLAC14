-- Products and Sums
-- 1

-- 2.a
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- 2.b
swapB :: (a, b) -> (b, a)
swapB x = (snd x, fst x)

-- 2.c
swapC :: (a, b) -> (b, a)
swapC x = case x of
            (a, b) -> (b, a)

-- 3
half :: Int -> Either Int Int
half x | even x     = Left k
       | otherwise  = Right k
       where k = div x 2

-- 4.a (a, b) -> (b, a)
-- 4.b (tx -> tx -> r) -> tx -> r
-- 4.c (ty -> r) -> (tz -> r) -> Either ty tz -> r
