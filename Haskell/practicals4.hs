-- 4.1
mul :: Int -> Int -> Int
mul 0 n = 0
mul m n = n + mul (m - 1) n

-- 4.2
myMin :: Int -> Int -> Int
myMin 0 n = n
myMin m 0 = m
myMin m n = 1 + myMin (m - 1) (n - 1)

-- 4.3
elemAt :: Int -> [a] -> a
elemAt 0 (x:xs) = x
elemAt n (x:xs) = elemAt (n - 1) xs

-- 4.4
insertAt :: Int -> a -> [a] -> [a]
insertAt 0 e xs = e : xs
insertAt n e (x:xs) = x : insertAt (n - 1) e xs