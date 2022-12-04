{-# LANGUAGE RankNTypes #-}

module Main where

import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.Either (lefts, isLeft, rights)

data Range a = Range {
    min :: a,
    max :: a
} deriving (Show) 

data ParseError = MaxNotGreaterThanMin | RangeSyntaxError | SplitError deriving (Show)

mkRange :: forall a. Ord a => a -> a -> Either (Range a) ParseError
mkRange a b
    | a <= b = Left (Range a b)
    | otherwise = Right MaxNotGreaterThanMin

parseRangeParts :: forall a. (Ord a, Read a) => String -> String -> Either (Range a) ParseError
parseRangeParts left right = case (readMaybe left, readMaybe right) of
    (Just rmin, Just rmax) -> mkRange rmin rmax
    (_, _) -> Right RangeSyntaxError

parseRange :: T.Text -> Either (Range Integer) ParseError
parseRange r = case T.split (=='-') r of
    (left : right : []) -> parseRangeParts (T.unpack left) (T.unpack right)
    _ -> Right SplitError 

parseLine :: T.Text -> Either (Range Integer, Range Integer) ParseError
parseLine r = case (parseRange <$> (T.split (==',') r)) of
    Left pairLeft : Left pairRight : [] -> Left (pairLeft, pairRight)
    Right pairLeft : _ : [] -> Right pairLeft
    _ : Right pairRight : [] -> Right pairRight
    _ -> Right SplitError

fullOverlap :: forall a. Ord a => Range a -> Range a -> Bool
fullOverlap (Range min1 max1) (Range min2 max2) = min2 <= min1 && max1 <= max2 || min1 <= min2 && max2 <= max1 

main :: IO ()
main = do
    inputLines <- lines <$> getContents
    let parsed = parseLine . T.pack <$> inputLines
    if (all isLeft parsed) 
        then print (length (filter (\(x, y) -> fullOverlap x y) (lefts parsed)))
        else print . rights $ parsed