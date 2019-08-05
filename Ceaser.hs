module Ceaser where

import           Data.Char

freqTable :: [Float]
freqTable =
    [ 8.1
    , 1.5
    , 2.8
    , 4.2
    , 12.7
    , 2.2
    , 2.0
    , 6.1
    , 7.0
    , 0.2
    , 0.8
    , 4.0
    , 2.4
    , 6.7
    , 7.5
    , 1.9
    , 0.1
    , 6.0
    , 6.3
    , 9.0
    , 2.8
    , 1.0
    , 2.4
    , 0.2
    , 2.0
    , 0.1
    ]

encode :: Int -> String -> String
encode n = map (shift n)

crack :: String -> String
crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [ chisqr (rotate n table') freqTable | n <- [0 .. 25] ]
    table' = frequency xs

positions :: Eq a => a -> [a] -> [Int]
positions e xs = [ i | (x, i) <- zip xs [0 ..], e == x ]

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [ ((o - e) ^ (2 :: Int)) / e | (o, e) <- zip os es ]

frequency :: String -> [Float]
frequency s =
    [ fromIntegral (count c s) * 100.0 / fromIntegral (length s)
    | c <- ['a' .. 'z']
    ]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

count :: Char -> String -> Int
count c s = sum $ map (\a -> if a == c then 1 else 0) s

shift :: Int -> Char -> Char
shift n c | isLower c = i2c $ (c2i c + n) `mod` 26
          | otherwise = c

c2i :: Char -> Int
c2i c = ord c - ord 'a'
i2c :: Int -> Char
i2c i = chr $ i + ord 'a'
