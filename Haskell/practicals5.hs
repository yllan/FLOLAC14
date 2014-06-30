data ETree a = Tip a | Bin (ETree a) (ETree a)

-- 1.a Only leaves contain labels.
-- 1.b
minET :: ETree Int -> Int
minET (Tip a) = a
minET (Bin ta tb) = min (minET ta) (minET tb)

-- 2
data Tree a = Null | Node a (Tree a) (Tree a)

minT :: Tree Int -> Int
minT Null = maxBound
minT (Node a tl tr) = min a $ min (minT tl) (minT tr)

-- 3
mapT :: (a -> b) -> Tree a -> Tree b
mapT f Null = Null
mapT f (Node a tl tr) = (Node (f a) (mapT f tl) (mapT f tr))

-- 4
flatten :: Tree a -> [a]
flatten Null = []
flatten (Node a tl tr) = flatten tl ++ [a] ++ flatten tr

-- 5.a
memberT :: Ord a => a -> Tree a -> Bool
memberT e Null = False
memberT e (Node x t u) 
        | e < x     = memberT e t
        | e > x     = memberT e u
        | otherwise = True

-- 5.b
insertT :: Ord a => a -> Tree a -> Tree a
insertT e Null = Node e Null Null
insertT e (Node x t u) 
        | e < x     = Node x (insertT e t) u
        | e > x     = Node x t (insertT e u)
        | otherwise = (Node x t u)
        