import ExprT

-- ex1

eval :: ExprT -> Integer
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b
eval (Lit a) = a

