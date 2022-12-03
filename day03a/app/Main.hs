module Main where

import qualified Data.Set as S
import GHC.Base ( ord ) 

toPrio :: Char -> Int
toPrio c = do
    let ic = ord c
    if 65 <= ic && ic <= 90 then
        ic - ord 'A' + 27
    else
        ic - ord 'a' + 1

score :: String -> Int
score line = do
    let prio = toPrio <$> line
    let itCount = ((`div` 2) . length) prio
    let (lc, rc) = splitAt itCount prio
    sum (S.intersection (S.fromList lc) (S.fromList rc))

main :: IO ()
main = do
    inputLines <- lines <$> getContents
    putStrLn (show (sum (score <$> inputLines)))