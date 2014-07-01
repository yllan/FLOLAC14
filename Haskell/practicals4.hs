data N = Zero | Succ N deriving Show

add :: N -> N -> N
add Zero n = n
add (Succ m) n = Succ (add m n)

-- 4.1
mul :: N -> N -> N
mul Zero n = Zero
mul (Succ m) n = add n (mul m n)

-- 4.2
myMin :: N -> N -> N
myMin Zero n = Zero
myMin m Zero = Zero
myMin (Succ m) (Succ n) = Succ (myMin m n)

-- 4.3
elemAt :: N -> [a] -> a
elemAt Zero (x:xs) = x
elemAt (Succ n) (x:xs) = elemAt n xs

-- 4.4
insertAt :: N -> a -> [a] -> [a]
insertAt Zero e xs = e : xs
insertAt (Succ n) e (x:xs) = x : insertAt n e xs