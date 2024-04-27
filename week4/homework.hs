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

fun2''' :: Integer -> Integer
fun2''' = sum . takeWhile (/= 1) . iterate (\x -> if even x then x `div` 2 else (3 * x + 1) `div` 2)


fun2'''' = iterate (\x -> if even x then x `div` 2 else (3 * x + 1) `div` 2)

fun2''''' = sum . filter even . takeWhile (>1) . iterate hailstone
  where hailstone n = if even n then n `div` 2 else 3*n + 1

tester :: (Integer -> Integer) -> (Integer -> Integer) -> Integer -> Bool
tester f f' x = f x == f' x

-- 二重にeven すればいいのか〜なるほど
-- hailstone の値が奇数になら
-- ないこともあることで悩んでたけど、そこで分離するのね〜
-- どっちの方がいいかは別として、hailstone というメガネを持ってたらこういう実装もできるな
-- どうやってfun2 通りの実装をするか考えてしまっていた
-- なかなか難しい

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

-- [each i lst | i <- [1..length lst]]

toBiTree :: [a] -> Integer -> Tree a
toBiTree [] _ = Leaf
toBiTree (x:xs) height = Node height (toBiTree head (height + 1)) x (toBiTree tail (height + 1))
    where
        half_len = length xs `div` 2
        head = take half_len xs
        tail = drop half_len xs
        

foldTree :: [a] -> Tree a
foldTree xs = toBiTree xs 0