module Main where

import qualified Data.Set as S

takeExact :: Int -> [a] -> Either String [a]
takeExact amt arr = do
    let h = take amt arr
    case amt == length h of
        True -> Right h
        False -> Left "Length error"

packetStart :: String -> Either String Int
packetStart t  = do
    head <- takeExact 14 t
    case (length head) == (length . S.fromList $ head) of
        True -> Right 14
        False -> (+1) <$> (packetStart (drop 1 t))

main :: IO ()
main = do
    input <- getContents 
    print . packetStart $ input