module Chap8 where

data Nat = Z | Succ Nat deriving Show

add :: Nat -> Nat -> Nat
add Z        n = n
add (Succ a) b = add a (Succ b)

n2i :: Nat -> Int
n2i Z        = 0
n2i (Succ n) = 1 + n2i n

mult :: Nat -> Nat -> Nat
mult Z        _ = Z
mult (Succ Z) b = b
mult (Succ a) b = add b (mult a b)

data Tree a = Leaf a | Node (Tree a ) a (Tree a) deriving Show

-- Note that this function searches at most the half of whole nodes.
occurs :: Ord a => a -> Tree a -> Bool
occurs e (Leaf v    ) = e == v
occurs e (Node l v r) = case compare e v of
    EQ -> True
    LT -> occurs e l
    GT -> occurs e r

data Btree a = Bleaf a | Bnode (Btree a) (Btree a) deriving Show
balanced :: Btree a -> Bool
balanced (Bleaf _) = True
balanced (Bnode l r) =
    abs (leafCount l - leafCount r) <= 1 && balanced l && balanced r

leafCount :: Btree a -> Int
leafCount (Bleaf _  ) = 1
leafCount (Bnode l r) = leafCount l + leafCount r

balance :: [a] -> Btree a
balance [a] = Bleaf a
balance xs  = Bnode (balance l) (balance r)
  where
    h = length xs `div` 2
    l = take h xs
    r = drop h xs

data Expr = Val Int | Add Expr Expr deriving Show

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val n  ) = f n
folde f g (Add a b) = g (folde f g a) (folde f g b)

eval :: Expr -> Int
eval = folde id (+)

-- instance Eq a => Eq (Maybe a) where
--     (==) Nothing  Nothing  = True
--     (==) Nothing  _        = False
--     (==) _        Nothing  = False
--     (==) (Just l) (Just r) = l == r
