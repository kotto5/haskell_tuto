import ExprT
import Parser

-- ex1

eval :: ExprT -> Integer
eval (Add a b) = eval a + eval b
eval (Mul a (Add b c)) = eval a * eval b + eval c -- 掛け算を優先する
eval (Mul a b) = eval a * eval b
eval (Lit a) = a

-- ex2
evalStr :: String -> Maybe Integer
evalStr exp = 
    case result of
        (Just a) -> Just (eval a)
        Nothing -> Nothing
    where result = parseExp Lit Add Mul exp
