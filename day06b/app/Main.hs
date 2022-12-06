module Main where

import qualified Data.Set as S

takeExact :: Int -> [a] -> Either String [a]
takeExact amt arr = do
    let h = take amt arr
    if amt == length h then Right h else Left "Length error"

packetStart :: String -> Either String Int
packetStart t  = do
    head <- takeExact 14 t
    if (length head) == (length . S.fromList $ head) then Right 14 else (+1) <$> (packetStart (drop 1 t))

main :: IO ()
main = do
    input <- getContents 
    print . packetStart $ input