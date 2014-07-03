import Data.Maybe

-- 2
hasZero :: [Int] -> Bool
hasZero [] = False
hasZero (x:xs) = (x == 0) || hasZero xs

-- 3.a
myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

-- 3.b crash

-- 3.c
myLastMaybe :: [a] -> Maybe a
myLastMaybe [] = Nothing
myLastMaybe [x] = Just x
myLastMaybe (x:xs) = myLastMaybe xs

-- 4.a
pos :: Eq a => a -> [a] -> Int
pos x (y:ys) = if (x == y) then 0 else 1 + pos x ys

-- 4.b
posMaybe :: Eq a => a -> [a] -> Maybe Int
posMaybe x [] = Nothing
posMaybe x (y : ys)
            | (x == y)  = Just 0
            | otherwise = fmap (+1) (posMaybe x ys)

-- 5
myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ myConcat xs

-- 6
double :: [a] -> [a]
double [] = []
double (x:xs) = x : x : double xs

-- 7
interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave (x:xs) ys = x : interleave ys xs

-- 8
splitLR :: [Either a b] -> ([a], [b])
splitLR [] = ([], [])
splitLR (x:xs) = case x of
                    Left a -> (a : ls, rs)
                    Right b -> (ls, b : rs)
                 where (ls, rs) = splitLR xs

-- 9
fan :: a -> [a] -> [[a]]
fan x [] = [[x]]
fan x (y:ys) = (x : y : ys) : map (y:) (fan x ys)

-- 10
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = myConcat $ map (fan x) (perms xs)

-- 11
inits :: [a] -> [[a]]
inits [] = [[]]
inits (x:xs) = [] : map (x:) (inits xs)

tails :: [a] -> [[a]]
tails [] = [[]]
tails (x:xs) = (x : xs) : tails xs
