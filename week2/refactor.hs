{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

stringsToLogMessage :: [String] -> LogMessage
stringsToLogMessage (s1:s2:s3:ls)
    | s1 == "E" = LogMessage (Error (read s2)) (read s3) (unwords ls)
    | s1 == "W" = LogMessage Warning (read s2) (unwords (s3 : ls))
    | s1 == "I" = LogMessage Info (read s2) (unwords (s3 : ls))
    | otherwise = Unknown (unwords (s1 : s2 : s3 : ls))
stringsToLogMessage s = Unknown (unwords s)

parseMessage :: String -> LogMessage
parseMessage s = stringsToLogMessage (words s)

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

isRightBigLog :: LogMessage -> LogMessage -> Bool
isRightBigLog (LogMessage _ time1 _) (LogMessage _ time2 _) = time1 < time2
isRightBigLog _ _ = False

getLogOfTree :: MessageTree -> LogMessage
getLogOfTree Leaf = Unknown "Error"
getLogOfTree (Node _ l _) = l

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert input Leaf = Node Leaf input Leaf
insert input (Node left top right)
 | input `isRightBigLog` top    = if left == Leaf || getLogOfTree left `isRightBigLog` input
                                then Node (Node left input Leaf) top right
                                else Node (insert input left) top right
 | otherwise                    = if right == Leaf || input `isRightBigLog` getLogOfTree right
                                then Node left top (Node Leaf input right)
                                else Node left top (insert input right)

build :: [LogMessage] -> MessageTree
build []       = Leaf
build (msg:[]) = insert msg Leaf
build (msg:ls) = insert msg (build ls)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node t1 msg t2) = inOrder t1 ++ [msg] ++ inOrder t2

isLevelOver50 :: LogMessage -> Bool
isLevelOver50 (LogMessage (Error n) _ _) = n >= 50
isLevelOver50 _ = False

filterOver50 :: [LogMessage] -> [LogMessage]
filterOver50 msgs = filter isLevelOver50 msgs

onlyMsg :: [LogMessage] -> [String]
onlyMsg ((LogMessage _ _ msg):ls) = msg : onlyMsg ls
onlyMsg _ = []

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = onlyMsg (filter isLevelOver50 msgs)
