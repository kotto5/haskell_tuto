import Data.List (foldl')
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldl' (*) 1 . map (\x -> x - 2) . filter even 


-- the point is to use two pattern
-- 片方を切り捨てて同じ処理をするだけだったらfilter で済むんだけど、
-- そうじゃない

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

y :: (a -> a) -> a
y x = x (y x)

utilFun2 :: Integral a => (a -> a) -> a -> a
utilFun2 f n = if even n then n `div` 2 else f (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . takeWhile (/= 1) . iterate (y utilFun2)

fun2'' :: Integer -> Integer
fun2'' = sum . takeWhile (/= 1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

