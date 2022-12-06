module Main where

import Data.List (elemIndex)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Debug.Trace (trace)

data Err = CrateParseError | LabelParseError | LengthMismatch | MoveParseError | MoveSplitError | TaskParseError | TakeParseError | NoSuchLabelErr | SolveError deriving (Show)

parseCrateRow :: String -> Either Err [Maybe Char]
parseCrateRow ('[' : b : ']' : t) = do 
    inp <- parseCrateRow t
    Right ([Just b] ++ inp)
parseCrateRow (' ' : ' ' : ' ' : ' ' : t) = do
    inp <- parseCrateRow t
    Right ([Nothing] ++ inp)
parseCrateRow (' ' : t) = parseCrateRow t
parseCrateRow [] = Right []
parseCrateRow _ = Left CrateParseError

zipAppend :: [Maybe Char] -> [[Char]] -> Either Err [[Char]]
zipAppend (Just c : t) (lh : lt) = do
    rest <- zipAppend t lt
    Right ([[c] ++ lh] ++ rest) 
zipAppend (Nothing : t) (lh : lt) = do
    rest <- zipAppend t lt
    Right ([lh] ++ rest) 
zipAppend (Just c : t) [] = do
    rest <- zipAppend t []
    Right ([[c]] ++ rest) 
zipAppend (Nothing : t) [] = do
    rest <- zipAppend t []
    Right ([[]] ++ rest) 
zipAppend [] [] = do
    Right [] 
zipAppend _ _ = Left LengthMismatch

parseInitial :: [String] -> Either Err [[Char]]
parseInitial (h : t) = do
    row <- parseCrateRow h
    rest <- parseInitial t
    zipAppend row rest
parseInitial [] = Right []

parseLabels :: String -> Either Err [Char]
parseLabels (' ' : l : ' ' : t) = do
    rest <- parseLabels t
    Right ([l] ++ rest)
parseLabels (' ' : t) = parseLabels t
parseLabels [] = Right []
parseLabels _ = Left LabelParseError

maybeToRight :: e -> Maybe a -> Either e a
maybeToRight _ (Just j) = Right j
maybeToRight err Nothing  = Left err

doMove :: String -> [[Char]] -> [Char] -> Either Err [[Char]]
doMove move crates labels = case T.unpack <$> (T.splitOn (T.pack " ") (T.pack move)) of
    ("move" : cnt : "from" : (src : []) : "to" : (dst : []) : []) -> do 
        srcIdx <- maybeToRight NoSuchLabelErr (elemIndex src labels)
        dstIdx <- maybeToRight NoSuchLabelErr (elemIndex dst labels)
        moveCount <- maybeToRight TakeParseError (readMaybe cnt :: Maybe Int)
        let taken = take moveCount (crates !! srcIdx)
        let dropped = take srcIdx crates ++ [drop moveCount (crates !! srcIdx)] ++ drop (srcIdx + 1) crates
        Right (take dstIdx dropped ++ [reverse taken ++ (dropped !! dstIdx)] ++ drop (dstIdx + 1) dropped)
    (_ : _ : _ : _ : _ : _) -> Left MoveParseError
    _ -> Left MoveSplitError

solve :: [[Char]] -> [Char] -> [String] -> Either Err [Char]
solve boxes labels (t: ins) = do
    boxesNew <- doMove t boxes labels
    solve boxesNew labels ins
solve boxes labels [] = Right ((\(h:t) -> h) <$> boxes)

task :: [String] -> Either Err [Char]
task inLines = do
    let (p1, ("" : p2)) = break (=="") inLines
    let (lbl : revBoxes) = reverse p1
    boxes <- parseInitial . reverse $ revBoxes
    labels <- parseLabels lbl
    solve boxes labels p2

main :: IO ()
main = do
    inputLines <- lines <$> getContents
    print (task inputLines)