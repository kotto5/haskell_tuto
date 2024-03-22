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
