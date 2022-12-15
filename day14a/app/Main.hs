-- Note: Horribly slow and inefficient

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Text.ParserCombinators.Parsec
    ( Parser, char, optional, string, sepBy1, many1 )
import Text.Parsec.Language ( emptyDef ) 
import qualified Text.ParserCombinators.Parsec.Token as P
import Data.Functor.Identity ( Identity )
import Text.Parsec.Prim ( ParsecT, parse )
import Data.List (foldl')
import qualified Data.Set as S
import Debug.Trace (trace)
import qualified Data.Vector as V

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser emptyDef

integer :: ParsecT String u Identity Integer
integer = P.integer lexer

whiteSpace :: ParsecT String u Identity ()
whiteSpace = P.whiteSpace lexer

data Tile = Air | Rock | Sand | Void deriving Eq

instance Show Tile where
    show Air = "."
    show Rock = "#"
    show Sand = "O"
    show Void = "X"

isSolid :: Tile -> Bool
isSolid Air = False
isSolid Rock = True
isSolid Sand = True
isSolid Void = False

type XY = (Int, Int)

parseVal :: Parser XY
parseVal = do
    x <- integer
    _ <- char ','
    y <- integer
    return (fromIntegral x, fromIntegral y)

parseSep :: Parser ()
parseSep = optional whiteSpace >> string "->" >> optional whiteSpace

newtype Path = Path [XY] deriving Show

parsePath :: ParsecT String () Identity Path
parsePath = Path <$> sepBy1 parseVal parseSep 

parsePathRows :: Parser [Path]
parsePathRows = many1 parsePath

lineTiles :: XY -> XY -> [XY]
lineTiles p1@(x1, y1) p2@(x2, y2)
    | x1 > x2 && y1 == y2 = [(x, y1) | x <- [x2..x1]]
    | x1 <= x2 && y1 == y2 = [(x, y1) | x <- [x1..x2]]
    | x1 == x2 && y1 > y2 = [(x1, y) | y <- [y2..y1]]
    | x1 == x2 && y1 <= y2 = [(x1, y) | y <- [y1..y2]]
    | otherwise = error $ "Invalid orthononal line: " ++ (show p1) ++ " -> " ++ (show p2)

getStoneTiles :: [Path] -> S.Set XY
getStoneTiles paths = S.fromList $ concat pathTiles
    where
        unpackedPaths = (\(Path x) -> x) <$> paths
        pathTiles =  fst . (\p -> foldl' (\(acc, lastHead) hh -> (acc ++ lineTiles lastHead hh, hh)) ([], head p) p) <$> unpackedPaths

nextState :: [[Tile]] -> Tile
nextState [[_, _, _, _],
           [_, _, Rock, _],
           [_, _, _, _]] = Rock
nextState [[_, _, Sand, _],
           [_, _, Air, _],
           [_, _, _, _]] = Sand
nextState [[_, _, _, Sand],
           [_, _, Air, isSolid -> True],
           [_, _, _, _]] = Sand
nextState [[_, Sand, _, _],
           [isSolid -> True, isSolid -> True, Air, _],
           [_, _, _, _]] = Sand
nextState [[_, _, _, _],
           [_, _, Air, _],
           [_, _, _, _]] = Air
nextState [[_, _, _, _],
           [_, _, Sand, _],
           [_, isSolid -> False, _, _]] = Air
nextState [[_, _, _, _],
           [_, _, Sand, _],
           [_, _, isSolid -> False, _]] = Air
nextState [[_, _, _, _],
           [_, _, Sand, _],
           [_, _, _, isSolid -> False]] = Air
nextState [[_, _, _, _],
           [_, _, Sand, _],
           [_, _, _, _]] = Sand
nextState e = error $ "Shape error: " ++ show e


stepSim :: [[Tile]] -> [[Tile]]
stepSim old = [[ nextState $ V.toList $ V.toList <$> (V.take 4 . V.drop x <$> (V.take 3 . V.drop y) oldWrap)  | (_, x) <- zip row [0..] ] | (row, y) <- zip old [0..]]
    where
        lvh = replicate (length $ head old) Void
        oldWrap = V.fromList $ V.fromList <$> (++ [Void]) . ([Void, Void] ++) <$> ([lvh] ++ old ++ [lvh])

spawnSand :: Int -> [[Tile]] -> [[Tile]]
spawnSand posX old = [[if x == posX && y == 0 then Sand else it | (it, x) <- zip row [0 :: Int ..] ] | (row, y) <- zip old [0 :: Int ..]]

stepSingle :: Int -> [[Tile]] -> [[Tile]]
stepSingle pourX oldState@(stepSim -> newState) = res
    where 
        res = if oldState == newState 
            then 
                (trace (draw newState) (spawnSand pourX newState)) 
            else
                newState

draw :: [[Tile]] -> String
draw vmap = (unlines $ show <$> vmap)
            ++ "\nSand: " ++ (show . sum $ length . filter (==Sand) <$> vmap)

stepUntilLeaking :: Int -> Int -> [[Tile]] -> Int
stepUntilLeaking pourX currCnt curr = if nextCnt < currCnt then nextCnt else stepUntilLeaking pourX nextCnt next 
    where
        next = stepSingle pourX curr
        nextCnt = sum $ length . filter (==Sand) <$> next

main :: IO ()
main = do
    inputLines <- getContents
    let Right pathRows = parse parsePathRows "Input data" inputLines
    let stoneTilesSet = getStoneTiles pathRows
    let stoneTiles = S.toList $ stoneTilesSet
    let minX = (minimum $ fst <$> stoneTiles) - 1
    let maxX = (maximum $ fst <$> stoneTiles) + 1
    let _minY = (minimum $ snd <$> stoneTiles) - 1
    let maxY = (maximum $ snd <$> stoneTiles) + 1

    let pourX = 500 - minX

    let vmap = [[ if S.member (x + minX, y) stoneTilesSet then Rock else Air | x <- [0..(maxX - minX)]] | y <- [0..maxY]]

    print (stepUntilLeaking pourX 0 vmap)