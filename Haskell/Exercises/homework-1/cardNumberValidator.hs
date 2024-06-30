-- Example
-- [1,3,8,6]

-- Exercise 1
{- 
 Example: toDigits 1234 == [1,2,3,4]
 Example: toDigitsRev 1234 == [4,3,2,1]
 Example: toDigits 0 == []
 Example: toDigits (-17) == []
-}
toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0 = []
    | n < 10 = [n]
    | otherwise = toDigits (div n 10) ++ [mod n 10]

reverseList :: [Integer] -> [Integer]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverseList (toDigits x)

-- Exercise 2
{- 
 Example: doubleEveryOther [8,7,6,5] == [16,7,12,5]
 Example: doubleEveryOther [1,2,3] == [1,4,3]
-}
doubleEveryOtherFromLeftToRight :: [Integer] -> [Integer]
doubleEveryOtherFromLeftToRight [] = []
doubleEveryOtherFromLeftToRight [x] = [x]
doubleEveryOtherFromLeftToRight (x:(y:zs)) = [x, 2*y] ++ doubleEveryOtherFromLeftToRight zs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther x =  reverseList (doubleEveryOtherFromLeftToRight (reverseList x))

-- Exercise 3
{- 
 Example: sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
-}
breakNumberIntoDecimalDigits :: Integer -> [Integer]
breakNumberIntoDecimalDigits n
    | n < 10 = [n]
    | otherwise = [div n 10, mod n 10]

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = x
sumDigits (x:xs) = sum (breakNumberIntoDecimalDigits x ++ [sumDigits xs])


-- Exercise 4
{- 
 Example: validate 4012888888881881 = True
 Example: validate 4012888888881882 = False
-}
validate :: Integer -> Bool
validate n = mod (sumDigits (doubleEveryOther (toDigits n))) 10 == 0