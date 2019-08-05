module Luhn where

luhnDouble :: Int -> Int
luhnDouble n = if d > 9 then d - 9 else d where d = n * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = n `mod` 10 == 0 where n = luhnDouble a + b + luhnDouble c + d
