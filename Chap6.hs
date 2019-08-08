module Chap6 where

fac :: Int -> Int
fac 0 = 1
fac n | n > 0     = n * fac (n - 1)
      | otherwise = error "cannot calculate factorial of negative int"

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

-- 2 ^ 3 = 2 * 2 ^ 2 = 2 * 2 * 2 ^ 1 = 2 * 2 * 2 * 1 = 8
(^) :: Int -> Int -> Int
(^) _ 0 = 1
(^) b p = b * (Chap6.^) b (p - 1)

euclid :: Int -> Int -> Int
euclid a b | a == b    = a
           | a > b     = euclid b a
           | otherwise = euclid a (b - a)

and' :: [Bool] -> Bool
and' [] = True
and' (x : xs) | x         = and' xs
              | otherwise = False

concat' :: [[a]] -> [a]
concat' []       = []
concat' (x : xs) = x ++ concat' xs


replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x | n > 0     = x : replicate' (n - 1) x
               | otherwise = error "negative replication"

(!!) :: [a] -> Int -> a
(!!) []       _ = error "no index"
(!!) (x : _ ) 0 = x
(!!) (_ : xs) n = (Chap6.!!) xs (n - 1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' e (x : xs) | x == e    = True
                 | otherwise = elem' e xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge xss@(x : xs) yss@(y : ys) | x > y     = y : merge xss ys
                                | otherwise = x : merge xs yss

halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs) where n = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort l = merge (f a) (f b)
  where
    (a, b) = halve l
    f xs | length xs >= 2 = msort xs
         | otherwise      = xs

sum' :: Num a => [a] -> a
sum' []       = 0
sum' (x : xs) = x + sum' xs

take' :: Int -> [a] -> [a]
take' 0 _        = []
take' _ []       = error "empty"
take' n (x : xs) = x : take' (n - 1) xs

last' :: [a] -> a
last' []       = error "empty"
last' [x     ] = x
last' (_ : xs) = last' xs
