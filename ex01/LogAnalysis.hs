{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

-- parseMessage :: String -> LogMessage
-- parseMessage () =

ex00 :: MessageType
ex00 = Info

ex01 :: MessageType
ex01 = Error 2 5

ex02 :: MessageType -> MessageType
ex02 m = m