{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

-- parseMessage :: String -> LogMessage
-- parseMessage () =

ex00 :: MessageType
ex00 = Info

ex01 :: MessageType
ex01 = Error 2

ex02 :: MessageType -> MessageType
ex02 m = m

data FallibleMessageType    = Failure
                            | Ok MessageType
    deriving Show

-- stringToMessageType :: String -> FallibleMessageType
-- stringToMessageType s
--  | s == "I" = Ok Info
--  | s == "W" = Ok Warning
--  | s == "E" = Ok (Error Int)
--  | otherwise = Failure

data MessageTypeAndStrings = MessageTypeAndStrings1 MessageType [String]

-- splitMessageTypeAndStrings :: [String] -> MessageTypeAndStrings
-- splitMessageTypeAndStrings (s1: s2: ls)
--     | s1 == "E" = MessageTypeAndStrings1 (Error (read s2)) ls
--     | s1 == "W" = MessageTypeAndStrings1 Warning (s2 : ls)
--     | s1 == "I" = MessageTypeAndStrings1 Info (s2 : ls)

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

-- 同じ値はどうするの? -> 一定のルールで裁かれるので問題なし

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

-- 挿入の条件
-- top left right input の 4種類
-- input < top && input > left -> Node (Node left input Leaf) top right
-- input < top && left == Leaf -> Node (Node Leaf input Leaf) top right
-- input > top && input < right -> Node left top (Node Leaf input right)
-- input > top && right == Leaf -> Node left top (Node Leaf input Leaf)

-- こうまとめられる
-- input < top && (input > left || left == Leaf) -> Node (Node left input Leaf) top right
-- input > top && (input < right || right == Leaf) -> Node left top (Node Leaf input right)

build :: [LogMessage] -> MessageTree
build []       = Leaf
build (msg:[]) = insert msg Leaf
build (msg:ls) = insert msg (build ls)

