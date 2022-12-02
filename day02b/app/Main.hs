module Main where

score:: String -> Int
score "B X" = 1
score "C X" = 2
score "A X" = 3
score "A Y" = 4
score "B Y" = 5
score "C Y" = 6
score "C Z" = 7
score "A Z" = 8
score "B Z" = 9
score _ = error "Invalid input"

main :: IO ()
main = do
    inputLines <- lines <$> getContents
    putStrLn (show (sum (score <$> inputLines)))