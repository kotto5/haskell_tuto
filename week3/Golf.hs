module Golf where
import Data.ByteString (split)

everyElement :: Int -> [a] -> [a]
everyElement _ [] = []
everyElement n ls = 
    let (ls1, ls2) = splitAt n ls
    in head ls1 : everyElement n ls2


-- skips :: [a] -> [[a]]
-- [a] -> [[a | filter (a % n) a ]]

-- everyElement ls = map head (map (drop (n - 1)) ls)

skips :: [a] -> [[a]]
skips [] = []
skips xs = map (\n -> everyNth n xs) [1..length xs]
    where everyNth n = map snd . filter (\(i, _) -> i `mod` n == 0) . zip [1..]




-- skips l = 

-- skip
-- every 1 element, every 2 element...
