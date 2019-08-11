module Chap7 where

import           Binary
import           Luhn

all' :: (a -> Bool) -> [a] -> Bool
all' f = foldr (\e a -> a && f e) True

-- TODO: cannot use on infinite list
any' :: (a -> Bool) -> [a] -> Bool
any' f = foldl (\a e -> a || f e) False

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []       = []
takeWhile' f (x : xs) = if f x then x : takeWhile' f xs else []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ []         = []
dropWhile' f a@(x : xs) = if f x then dropWhile' f xs else a

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\e a -> (f e) : a) []


filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\e a -> if p e then e : a else a) []

dec2int :: [Int] -> Int
dec2int = foldl (\a e -> a * 10 + e) 0

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f t = f (fst t) (snd t)


unfold :: ([a] -> Bool) -> ([a] -> b) -> ([a] -> [a]) -> [a] -> [b]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

uchop8 :: [Bit] -> [[Bit]]
uchop8 = unfold p h t
  where
    p = null
    h = take 8
    t = drop 8

umap :: (a -> b) -> [a] -> [b]
umap f = unfold p h t
  where
    p = null
    h = f . head
    t = tail

uiterate :: (a -> a) -> a -> [a]
uiterate f e = unfold p h (\x -> [f (head x)]) [e]
  where
    p :: (a -> Bool)
    p _ = False
    h :: ([a] -> a)
    h = head

count :: Eq a => a -> [a] -> Int
count x = foldl (\a e -> if e == x then a + 1 else a) 0

pencode :: String -> [Bit]
pencode xs = b : c
  where
    c = encode xs
    b = i2b $ count O c `mod` 2

pdecode :: [Bit] -> String
pdecode xs | length d `mod` 8 /= 0 = error "data lost"
           | calc == parity        = decode d
           | otherwise             = error "parity mismatch"
  where
    parity = head xs
    d      = tail xs
    calc   = i2b $ count O d `mod` 2

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _  _  []       = []
altMap f1 f2 (x : xs) = f1 x : altMap f2 f1 xs

luhn :: [Int] -> Bool
luhn xs = n `mod` 10 == 0 where n = sum $ altMap luhnDouble id xs
