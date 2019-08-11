module AbsMachine where

data Expr = Val Int | Add Expr Expr deriving (Show)

type Cont = [Op]
data Op = EVAL Expr | ADD Int deriving Show

eval :: Expr -> Cont -> Int
eval (Val n  ) cs = exec cs n
eval (Add a b) cs = eval a (EVAL b : cs)

exec :: Cont -> Int -> Int
exec []            n = n
exec (EVAL e : cs) n = eval e cs + n
exec (ADD  n : cs) m = exec cs (n + m)

value :: Expr -> Int
value e = eval e []
