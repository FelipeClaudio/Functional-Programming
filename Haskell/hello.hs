main = do
  putStrLn "Hello, everybody!"
  putStrLn ("Please look at my favorite odd numbers: " ++ show (filter odd [10..20]))

x :: Int
x = 3

-- Code using guards
fib :: Integer -> Integer
fib n
  | n < 0       = 0
  | n == 1      = 1
  | otherwise   = fib(n-1) + fib(n-2)

-- Code using cases
fib2 :: Integer -> Integer
fib2 0          = 0
fib2 1          = 1
fib2 n          = fib2(n-1) + fib2(n-2)

-- Array creation
-- Remember that the variables are immutable
array1 = [2,3,4]

-- Array creation using the cons operator ":" 
array2 = 2 : 3 : 4 : []
areEqual = array1 == array2

-- Compute the length of a list of Integers.
intListLength :: [Integer] -> Integer
intListLength []     = 0
-- This codes matches a list of elements like [x, xs] 
-- where xs is a list of remaining elements and recursively adds 1 to the counting of elements
-- It could also be intListLength (_:xs) as x is not being used
intListLength (x:xs) = 1 + intListLength xs

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []         = []     -- Do nothing to the empty list
sumEveryTwo (x:[])     = [x]    -- Do nothing to lists with a single element
-- the expression below is equivalent to this array [x, y, zs] where zs is another array with the remaining elements
sumEveryTwo (x:(y:zs)) = (x + y) : sumEveryTwo zs

fibSeqArray :: Integer -> [Integer]
fibSeqArray n = map fib [1..n]