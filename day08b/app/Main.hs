module Main where

import Data.List (transpose)

scenicScore :: [Int] -> [Int]
scenicScore (t:xs) = [(\(a, b) -> if take 1 b /= [] then ((+1) . length $ a) else (length a)) . (break (>= t)) $ xs] ++ scenicScore xs 
scenicScore [] = []
    
main :: IO ()
main = do
    mp <- ((read . (:[]) <$>) <$>) <$> lines <$> getContents
    print $ maximum $ maximum <$> (foldl1 (zipWith (zipWith (*))) (($ mp) <$> [(scenicScore <$>), (reverse . scenicScore . reverse <$>), transpose . (scenicScore <$>) . transpose, transpose . (reverse . scenicScore . reverse <$>) . transpose]))