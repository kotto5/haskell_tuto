hailstone :: Integer -> Integer
hailstone n
 | n `mod` 2 == 0 = n `div` 2
 | otherwise      = 3*n + 1

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

intListLength :: [Integer] -> Integer
intListLength []     = 0
intListLength (x:xs) = 1 + intListLength xs

hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n) - 1

undoubled :: [Integer] -> [Integer]

validateCredit :: [Integer] -> boolean
validateCredit = sum undoubled doubleOfEverySecond 
