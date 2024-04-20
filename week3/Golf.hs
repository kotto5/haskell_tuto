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

