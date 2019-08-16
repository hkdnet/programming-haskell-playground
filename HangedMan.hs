import           System.IO

getCh :: IO Char
getCh = do
    hSetEcho stdin False
    c <- getChar
    hSetEcho stdin True
    return c

sgetLine :: IO String
sgetLine = do
    x <- getCh
    if x == '\n'
        then do
            putChar x
            return []
        else do
            putChar '-'
            xs <- sgetLine
            return (x : xs)

match :: String -> String -> String
match actual expected = map f $ zip actual expected
    where f (a, b) = if a == b then a else '-'

play :: String -> IO ()
play word = do
    putStr "> "
    guess <- getLine
    if guess == word
        then putStrLn "You got it!"
        else do
            putStrLn "Wrong"
            putStr "Hint: "
            putStrLn $ match word guess
            play word

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Think of a word:"
    word <- sgetLine
    play word
    return ()
