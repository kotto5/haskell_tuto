-- class Eq' a where
--     (==) :: a -> a -> Bool
--     (/=) :: a -> a -> Bool
--     x /= y = not (x == y)

-- data Foo = F Int | G Char

-- instance Eq' Foo where
--     (F i1) == (F i2) = i1 == i2
--     (G c1) == (G c2) = c1 == c2
--     _ == _ = false

-- test :: Eq' Foo -> Eq' Foo -> Bool
-- test a b = a (==) b

class Listable a where
    toList :: a -> [Int]


instance Listable Int where
    toList :: Int -> [Int]
    toList x = [x]

instance Listable Bool where
    toList True = [1]
    toList False = [0]

instance Listable [Int] where
    toList = id

data Tree a = Empty | Node a (Tree a) (Tree a)

instance Listable (Tree Int) where
    toList Empty = []
    toList (Node x l r) = toList l ++ [x] ++ toList r

foo x y = sum (toList x) == sum (toList y) || x < y

fun1 :: Integer -> Integer -> Integer
fun1 x = foldl (+) 0 . replicate 5