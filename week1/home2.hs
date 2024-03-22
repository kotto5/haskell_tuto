type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 peg1 peg2 peg3 = (peg1, peg3) : []
hanoi n peg1 peg2 peg3 = hanoi (n-1) peg1 peg3 peg2 ++ hanoi 1 peg1 peg2 peg3 ++ hanoi (n-1) peg2 peg1 peg3

hanoi2 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi2 1 pegStart peg2 peg3 pegGoal = (pegStart, pegGoal) : []
hanoi2 3 pegStart peg2 peg3 pegGoal = (pegStart, peg2) : (pegStart, peg3) : (pegStart, pegGoal) : (peg3, pegGoal) : (peg2, pegGoal) : []
hanoi2 4 pegStart peg2 peg3 pegGoal = (pegStart, peg2) : (pegStart, peg3) : (pegStart, pegGoal) : (peg3, pegGoal) : (peg2, pegGoal) : []

hanoi2 n pegStart peg2 peg3 pegGoal 
 | n `mod` 2 == 0 = hanoi2 (n-1) pegStart peg2 pegGoal peg3 ++ hanoi2 1 pegStart peg2 peg3 pegGoal ++ hanoi2 (n-1) peg3 pegStart peg2 pegGoal
 | otherwise = hanoi2 (n-1) pegStart peg3 pegGoal peg2 ++ hanoi2 1 pegStart peg2 peg3 pegGoal ++ hanoi2 (n-1) peg2 pegStart peg3 pegGoal
