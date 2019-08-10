module Vote where

import           Data.List

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

rmdups :: Eq a => [a] -> [a]
rmdups []       = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

result :: Ord a => [a] -> [(Int, a)]
result vs = sort [ (count e vs, e) | e <- rmdups vs ]

winner :: Ord a => [a] -> a
winner = snd . last . result

ballots :: [[String]]
ballots =
    [ ["Red", "Green"]
    , ["Blue"]
    , ["Green", "Red", "Blue"]
    , ["Blue", "Green", "Red"]
    , ["Green"]
    ]

eliminate :: Eq a => a -> [[a]] -> [[a]]
eliminate e = map $ filter (/= e)

rmempty :: [[a]] -> [[a]]
rmempty = filter (not . null)

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' xs = case e of
    []      -> error "empty"
    [a    ] -> a
    (a : _) -> winner' $ eliminate a xs
    where e = rank $ rmempty xs
