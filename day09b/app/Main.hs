{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.List (mapAccumL)
import qualified Data.Map as M

data Direction = DirUp | DirDown | DirLeft | DirRight deriving(Show)

type XY = (Integer, Integer);

mkDir :: String -> Direction
mkDir "U" = DirUp
mkDir "D" = DirDown
mkDir "L" = DirLeft
mkDir "R" = DirRight
mkDir _ = error "Bad direction"

walk :: Direction -> XY -> XY
walk DirUp (x, y) = (x, y + 1)
walk DirDown (x, y) = (x, y - 1)
walk DirLeft (x, y) = (x - 1, y)
walk DirRight (x, y) = (x + 1, y)

pullTail :: XY -> XY -> XY
pullTail (x, y) (tx, ty)
    | abs (x - tx) <= 1 && abs (y - ty) <= 1 = (tx, ty)
    | y - ty == -2 && x - tx == -2 = (x + 1, y + 1)
    | y - ty == 2 && x - tx == -2 = (x + 1, y - 1)
    | y - ty == -2 && x - tx == 2 = (x - 1, y + 1)
    | y - ty == 2 && x - tx == 2 = (x - 1, y - 1)
    | y - ty == -2 = (x, y + 1)
    | y - ty == 2 = (x, y - 1)
    | x - tx == 2 = (x - 1, y)
    | x - tx == -2 = (x + 1, y)


pullMulti :: XY -> [XY] -> [XY]
pullMulti xy knots = do
    snd $ mapAccumL (\hxy txy -> (pullTail hxy txy, pullTail hxy txy)) xy knots

steps :: M.Map XY Integer -> XY -> [XY] -> [Direction] -> Int
steps mp xy txy (h:t) = do
    let newXY = (walk h xy)
    steps (M.alter (maybe (Just 1) (Just . (+1))) (last txy) mp) newXY (pullMulti newXY txy) t
steps mp _ txy [] = length $ M.elems $ M.alter (maybe (Just 1) (Just . (+1))) (last txy) mp

main :: IO ()
main = do
    macroSteps <- lines <$> getContents
    print $ steps M.empty (0, 0) (take 9 $ repeat (0, 0)) (foldl1 (++) $ ((\((mkDir -> dir) : (read -> num) : []) -> take num $ repeat dir) . words) <$> macroSteps)