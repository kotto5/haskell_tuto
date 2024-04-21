import Data.Char (toUpper, chr)
import Data.List (delete)
import Text.Printf (IsChar(toChar))

-- First list is the input itself, second list contains only every second
-- element of the input list, etc.
-- > skips "ABCD"        == ["ABCD", "BD", "C", "D"]
-- > skips "hello!"      == ["hello!", "el!", "l", "o", "!"]
-- > skips [1]           == [[1]]
-- > skips [True, False] == [[True,False], [False]]
-- > skips []            == []
skips :: [a] -> [[a]]
-- As mentioned in the specification for exercise 1, the output list is of the
-- same length as the input list.
skips lst = [each i lst | i <- [1..length lst]]

-- Returns each @n@-th element of the list.
-- > each 2 [1, 2, 3, 4] = [2, 4]
-- > each 2 [] = []
each :: Int -> [a] -> [a]
-- Using list comprehension. Index list returns exactly the indices of every
-- n-th element. Is safe because when the @lst@ is empty then the index list is
-- also empty. And indices go from 0 to (length lst) - 1, so also we don't
-- index anything to big.
each n lst = [lst !! i | i <- [n-1, n-1+n..length lst - 1]]

toUpperFunc s = [toUpper c | c <- s]

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs)
  | y > x && y > z = y : localMaxima (y:z:zs)
  | otherwise      = localMaxima (y:z:zs)
localMaxima _ = []

histogram :: [Integer] -> String
histogram lst = doHist lst ++ "==========\n0123456789\n"

doHist :: [Integer] -> String
doHist [] = ""
doHist lst = doHist rest ++ line (unique lst)
    where
        unique_lst = unique lst
        rest = diff lst unique_lst

-- [0,1,2] == "***       \n"
--             0123456789
line :: [Integer] -> String
line [] = "" 
line xs = [toHist i xs | i <- [0..9]] ++ "\n"

-- 1, [1] == '*'
-- 1, [0] == ' '
toHist :: Integer -> [Integer] -> Char
toHist i xs = 
    if i `elem` xs
    then '*'
    else ' '

unique :: [Integer] -> [Integer]
unique [] = []
unique (x:xs) = x : unique (filter (/=x) xs)

diff :: [Integer] -> [Integer] -> [Integer]
diff (x:xs) ys =
    if x `elem` ys
    then diff xs (delete x ys)
    else x : diff xs ys
diff [] _ = []

-- " * * \n==========\n0123456789\n"
-- "   * *    \n==========\n0123456789\n"
-- "0123456789"