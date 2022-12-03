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

chunks :: [a] -> [[a]]
chunks []           = []
chunks (h1:h2:h3:t) = [h1,h2,h3] : chunks t
chunks _            = error "List len not divisible by 3"

main :: IO ()
main = do
    inputLines <- lines <$> getContents
    let groups = (S.fromList <$>) <$> chunks ((toPrio <$>) <$> inputLines)
    let s = sum (sum <$> S.toList <$> (foldl1 S.intersection <$> groups))
    putStrLn (show s)