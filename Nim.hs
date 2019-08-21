import           Data.Char
import           System.IO

type Board = [Int]

next :: Int -> Int
next 1 = 2
next 2 = 1
next _ = error "unknown player"

showLine :: Int -> Int -> IO ()
showLine l n = do
    putStr $ show l
    putChar ' '
    putStrLn $ replicate n '*'

showBoard :: Board -> IO ()
showBoard = showBoard' 0
  where
    showBoard' _ []       = return ()
    showBoard' n (x : xs) = do
        showLine n x
        showBoard' (n + 1) xs

initBoard :: [Int]
initBoard = [5, 4, 3, 2, 1]

finished :: Board -> Bool
finished = all (0 ==)

valid :: Board -> Int -> Int -> Bool
valid b i n = b !! i >= n

move :: Board -> Int -> Int -> Board
move b i n | valid b i n = [ update idx x | (idx, x) <- zip [0 ..] b ]
           | otherwise   = error "out of bound"
    where update idx x = if idx == i then x - n else x

getDigit :: String -> IO Int
getDigit prompt = do
    putStr prompt
    x <- getChar
    _ <- getChar -- for skip \n
    if isDigit x
        then return (digitToInt x)
        else do
            putStrLn "ERROR: Invalid digit"
            getDigit prompt

play :: Int -> Board -> IO ()
play p b = do
    putStr "Player "
    putStr $ show p
    putStrLn "'s turn:"
    showBoard b
    (r, n) <- f
    let newBoard = move b r n
    if finished newBoard
        then putStrLn "win"
        else do
            let nextp = next p
            play nextp newBoard

  where
    f = do
        r <- getDigit "row: "
        n <- getDigit "num: "
        if valid b r n
            then return (r, n)
            else do
                putStrLn "invalid row, num pair"
                f


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin  LineBuffering
    putStrLn "Start Nim!"
    play 1 initBoard
    return ()
