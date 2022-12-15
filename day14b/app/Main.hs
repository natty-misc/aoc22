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
import qualified Data.Map as M
import Data.Maybe (isJust, isNothing, fromJust)

data Tile = Stone | Sand deriving Eq

instance Show Tile where
    show Stone = "#"
    show Sand = "O"

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser emptyDef

integer :: ParsecT String u Identity Integer
integer = P.integer lexer

whiteSpace :: ParsecT String u Identity ()
whiteSpace = P.whiteSpace lexer

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
data TileMap = TileMap (M.Map XY Tile) Int deriving Show

(???) :: TileMap -> XY -> Maybe Tile
(???) (TileMap _ maxY) (_, (==maxY) -> True) = Just Stone
(???) (TileMap vmap _) xy = vmap M.!? xy 


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

findPlacement :: XY -> TileMap -> Maybe XY
findPlacement xy ((??? xy) -> Just _) = Nothing
findPlacement (x, y) tiles@((??? (x, y + 1)) -> Nothing) = findPlacement (x, y + 1) tiles
findPlacement xy@(x, y) tiles@((??? (x, y + 1)) -> Just _)
    | isNothing (tiles ??? (x - 1, y + 1)) = findPlacement (x - 1, y + 1) tiles
    | isNothing (tiles ??? (x + 1, y + 1)) = findPlacement (x + 1, y + 1) tiles
    | otherwise = Just xy
findPlacement xy _ = Just xy 


stepUntilFull :: XY -> TileMap -> TileMap
stepUntilFull spawnPos curr@(TileMap vcurr maxY)
    | isJust til = stepUntilFull spawnPos $ TileMap (M.insert (fromJust til) Sand vcurr) maxY
    | otherwise = curr
    where
        til = findPlacement spawnPos curr

main :: IO ()
main = do
    inputLines <- getContents
    let Right pathRows = parse parsePathRows "Input data" inputLines
    let stone = getStoneTiles pathRows
    let maxY = (maximum $ snd <$> S.toList stone) + 2
    let vmap = TileMap (M.fromSet (const Stone) stone) maxY

    print (length . filter (==Sand) $ M.elems . (\(TileMap a _) -> a) $ stepUntilFull (500, 0) vmap)