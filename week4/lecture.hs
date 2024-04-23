greaterThan100_2 xs = filter (\x -> x > 100) xs

myTest' :: [Integer] -> Bool
myTest' = even . length . greaterThan100_2
