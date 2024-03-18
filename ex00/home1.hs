toDigitsRev :: Integer -> [Integer]
toDigitsRev x
 | x <= 0       = []
 | otherwise    = x `mod` 10 : toDigits(x `div` 10)

toDigits    :: Integer -> [Integer]
toDigits list = toReverse (toDigitsRev list)

toReverse :: [Integer] -> [Integer]
toReverse []      = []
toReverse (x:xs)  = toReverse xs ++ [x]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x:xs)
 | length xs `mod` 2 == 1 = x * 2 : doubleEveryOther xs
 | otherwise = x : if length xs > 0 then doubleEveryOther xs else []