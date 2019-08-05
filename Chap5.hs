module Chap5 where

-- for convenience
pow2 :: Int -> Int
pow2 n = n ^ (2 :: Int)

pow2Sum :: Int -> Int
pow2Sum n = sum [ pow2 x | x <- [1 .. n] ]

grid :: Int -> Int -> [(Int, Int)]
grid m n = [ (x, y) | x <- [0 .. m], y <- [0 .. n] ]

square :: Int -> [(Int, Int)]
square n = [ cord | cord <- grid n n, uncurry (/=) cord ]

replicate :: Int -> a -> [a]
replicate n e = [ e | _ <- [1 .. n] ]

pyths :: Int -> [(Int, Int, Int)]
pyths n =
    [ (x, y, z)
    | x <- [1 .. n]
    , y <- [1 .. n]
    , z <- [1 .. n]
    , pow2 x + pow2 y == pow2 z
    ]

perfects :: Int -> [Int]
perfects n = [ x | x <- [1 .. n], isPerfect x ]
  where
    isPerfect x = (sum $ factors x) == x * 2
    factors num = [ x | x <- [1 .. num], num `mod` x == 0 ]

comp2 :: [(Int, Int)]
comp2 = [ (x, y) | x <- [1, 2, 3], y <- [4, 5, 6] ]

comp1 :: [(Int, Int)]
comp1 = concat [ [ (x, y) | y <- [4, 5, 6] ] | x <- [1, 2, 3] ]

find :: Eq a => a -> [(a, b)] -> [b]
find k t = [ v | (k', v) <- t, k == k' ]

positions :: Eq a => a -> [a] -> [Int]
positions e xs = find e $ zip xs [0 ..]

scalaProduct :: [Int] -> [Int] -> Int
scalaProduct xs ys = sum [ x * y | (x, y) <- zip xs ys ]
