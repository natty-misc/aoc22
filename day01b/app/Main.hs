{-# LANGUAGE RankNTypes #-}

module Main where

import Data.List

splitListPred:: (a -> Bool) -> [a] -> [[a]]
splitListPred _ [] = []
splitListPred pred items = do
    let (h, t) = break pred items
    [h] ++ splitListPred pred (drop 1 t)

splitListBySep:: forall a. Eq a => a -> [a] -> [[a]]
splitListBySep sep items = splitListPred (sep ==) items

readStdInToLines:: IO [String]
readStdInToLines = lines <$> getContents

main :: IO ()
main = do
    inputLines <- readStdInToLines
    let elvesStr = splitListBySep "" inputLines
    let elvesNum = ((read :: String -> Integer) <$>) <$> elvesStr;
    let elvesCal = (reverse . sort) (sum <$> elvesNum)
    putStrLn (show (sum (take 3 elvesCal)))