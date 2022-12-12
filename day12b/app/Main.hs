{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import qualified Data.HashSet as HS 
import qualified Data.Map as M 
import Data.Char (ord)
import Data.Maybe (fromJust)
import qualified Data.Graph.AStar as AS

type XY = (Int, Int)

heuristic :: XY -> XY -> Int
heuristic (goalX, goalY) (posX, posY) = abs (goalX - posX) + abs (goalY - posY)

parseRow :: XY -> [Char] -> (Maybe XY, Maybe XY, [Int])
parseRow (xx, yy) ('S' : xs) = (\(_, e, v) -> (Just (xx, yy), e, [ord 'a' - ord 'a'] ++ v)) $ parseRow (xx + 1, yy) xs
parseRow (xx, yy) ('E' : xs) = (\(s, _, v) -> (s, Just (xx, yy), [ord 'z' - ord 'a'] ++ v)) $ parseRow (xx + 1, yy) xs
parseRow (xx, yy) (c : xs) = (\(s, e, x) -> (s, e, [ord c - ord 'a'] ++ x)) $ parseRow (xx + 1, yy) xs
parseRow _ [] = (Nothing, Nothing, [])

orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just a) _ = Just a
orElse Nothing b = b

parseMap :: XY -> [[Char]] -> (Maybe XY, Maybe XY, [[Int]])
parseMap (xx, yy) (x : xs) = ((\(s, e, v) -> (\(ss, ee, vv) -> (s `orElse` ss, e `orElse` ee, [v] ++ vv)) $ parseMap (xx, yy + 1) xs) $ parseRow (xx, yy) x)
parseMap (_, _) [] = (Nothing, Nothing, [])

enumerate :: [[Int]] -> [[(XY, Int)]]
enumerate vMap = [[ ((colNr, rowNr), cell) | (colNr, cell) <- zip [0..] row ] | (rowNr, row) <- zip [0..] vMap]

mkEdges :: [[Int]] -> M.Map XY [XY] 
mkEdges (enumerate -> vMap) = M.fromListWith (++) edges
    where
        left = concat $ zipWith zip (drop 1 <$> vMap) vMap 
        right = concat $ zipWith zip vMap (drop 1 <$> vMap) 
        up = concat $ zipWith zip (drop 1 vMap) vMap 
        down = concat $ zipWith zip vMap (drop 1 vMap)
        edges = (\(fst -> src, fst -> dst) -> (src, [dst])) <$> filter (\((_, h1), (_, h2)) -> (h2 - h1) >= -1) (concat [left, right, up, down])

getNeighbors :: M.Map XY [XY] -> XY -> HS.HashSet XY
getNeighbors edges pos = HS.fromList $ fromJust ((M.lookup pos edges) `orElse` Just [])

aStar :: [[Int]] -> M.Map XY [XY] -> XY -> XY -> Maybe [XY]
aStar vmap edges start end = AS.aStar (getNeighbors edges) (const $ const 1) (const 0) (\(x, y) -> ((vmap !! y) !! x) == 0) end

main :: IO ()
main = do
    inputLines <- lines <$> getContents
    let (Just start, Just end, vmap) = (parseMap (0, 0) $ inputLines)
    print $ length <$> aStar vmap (mkEdges $ vmap) start end