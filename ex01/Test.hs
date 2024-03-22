module Test where

import LogAnalysis
import Log

-- testInsert :: Int -> LogMessage
-- testInsert n = map insert testParse parse n error.log
-- テスト関数の呼び方がわからないぜ!

easyLog :: Int -> LogMessage
easyLog n = LogMessage Info n "test"

testInsert :: MessageTree
testInsert = insert (easyLog 10) Leaf
-- testInsert = insert (easyLog 10) (insert (easyLog 9) Leaf)
-- testInsert = insert (easyLog 10) (insert (easyLog 9) (insert (easyLog 11)  Leaf))
-- testInsert = insert (easyLog 9) (insert (easyLog 10) (insert (easyLog 11)  Leaf))
-- testInsert = insert (easyLog 10) (insert (easyLog 9) (insert (easyLog 11) (insert (easyLog 8) Leaf)))

