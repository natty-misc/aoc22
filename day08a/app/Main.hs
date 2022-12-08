module Main where

import Data.List (transpose)

countIncreases :: [Int] -> [Bool]
countIncreases xs = fst $ foldl (\(acc_cnt, acc_val) (i_cnt, i_val) -> if i_val > acc_val then (acc_cnt ++ [True], i_val) else (acc_cnt ++ [False], acc_val)) ([], -1) ((\x -> (0, x)) <$> xs)

main :: IO ()
main = do
    map <- ((read <$>) <$>) <$> (((:[]) <$>) <$>) <$> lines <$> getContents
    print $ sum $ sum <$> (fromEnum <$>) <$> (foldl1 (zipWith (zipWith (||))) (($ map) <$> [(countIncreases <$>), (reverse . countIncreases . reverse <$>), transpose . (countIncreases <$>) . transpose, transpose . (reverse . countIncreases . reverse <$>) . transpose]))