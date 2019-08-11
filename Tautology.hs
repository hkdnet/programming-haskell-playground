module Tautology where

data Prop = Const Bool
    | Var Char
    | Not Prop
    | And Prop Prop
    | Imply Prop Prop deriving (Show)

--    A ^ not A
--    (A ^ B) -> A
--    A -> (A ^ B)
--    (A ^ (A -> B)) -> B

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))
p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

type Assoc a b = [(a, b)]

type Subst = Assoc Char Bool

find :: (e -> Bool) -> [e] -> e
find _ []       = error "missing"
find f (x : xs) = if f x then x else find f xs

eval :: Subst -> Prop -> Bool
eval _ (Const b  ) = b
eval s (Var   v  ) = snd (find (\t -> v == fst t) s)
eval s (Not   p  ) = not $ eval s p
eval s (And   a b) = eval s a && eval s b
eval s (Imply a b) = not (eval s a) || eval s b

vars :: Prop -> [Char]
vars (Const _  ) = []
vars (Var   c  ) = [c]
vars (Not   p  ) = vars p
vars (And   a b) = vars a ++ vars b
vars (Imply a b) = vars a ++ vars b

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = foldl (\acc e -> (True : e) : (False : e) : acc) [] $ bools (n - 1)

rmdups :: Eq a => [a] -> [a]
rmdups []       = []
rmdups (x : xs) = x : rmdups (filter (x /=) xs)

patterns :: [Char] -> [Subst]
patterns xs = map (zip xs) bss where bss = bools $ length xs

isTaut :: Prop -> Bool
isTaut p = all (\s -> eval s p) ss
  where
    vs = rmdups $ vars p
    ss = patterns vs
