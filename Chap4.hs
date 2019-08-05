module Chap4 where

halve :: [a] -> ([a], [a])
halve xs | even $ length xs = (take l xs, drop l xs)
         | otherwise        = error "odd length list"
    where l = length xs `div` 2

third1 :: [a] -> a
third1 xs = head $ tail $ tail xs

third2 :: [a] -> a
third2 xs = xs !! 2

third3 :: [a] -> a
third3 (_ : _ : x : _) = x
third3 _               = error "list should contain more than 2 elements."

safetail1 :: [a] -> [a]
safetail1 xs = if null xs then [] else tail xs

safetail2 :: [a] -> [a]
safetail2 xs | null xs   = []
             | otherwise = tail xs

safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 xs = tail xs
