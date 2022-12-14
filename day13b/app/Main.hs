{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Text.ParserCombinators.Parsec
    ( char, (<?>), (<|>), parse, ParseError, Parser )
import Text.Parsec.Language ( emptyDef ) 
import qualified Text.ParserCombinators.Parsec.Token as P
import Control.Arrow ( ArrowChoice(left) ) 
import Data.Functor.Identity ( Identity )
import Text.Parsec.Prim ( ParsecT )
import Data.List (sortBy, elemIndex)
import Data.Maybe ( fromJust )

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser emptyDef

decimal :: ParsecT String u Identity Integer
decimal = P.integer lexer

commaSep :: ParsecT String u Identity a -> ParsecT String u Identity [a]
commaSep = P.commaSep lexer

data Item = List [Item] | Val Integer deriving (Show, Eq)
type ItemPair = (Item, Item)

parseVal :: Parser Item
parseVal = do
    a <- decimal
    return $ Val a

parseList :: Parser Item
parseList = do
    _ <- char '['
    a <- commaSep parseItem
    _ <- char ']'
    return $ List a

parseItem :: Parser Item
parseItem = parseVal <|> parseList <?> error "Parse error"

data Err = ParsecError ParseError | SepError String | OtherParseError String deriving Show

parsePairs :: [String] -> Either Err [ItemPair]
parsePairs ((parse parseItem "left" -> l) : (parse parseItem "right" -> r) : "" : xs) = do
    ll <- left ParsecError l
    rr <- left ParsecError r
    rest <- parsePairs xs
    Right $ [(ll, rr)] ++ rest
parsePairs (_ : _ : s : _) = Left . SepError $ "Failed to parse empty line: " ++ s
parsePairs [] = Right []
parsePairs o = Left . OtherParseError $ "Failed to parse: " ++ show o

wellSorted :: ItemPair -> Ordering
wellSorted (List (lh:lxs), List (rh:rxs)) = if wh /= EQ then wh else (wellSorted $ (List lxs, List rxs)) where wh = wellSorted (lh, rh) 
wellSorted (Val lh, List rr) = wellSorted (List [Val lh], List rr)
wellSorted (List ll, Val rh) = wellSorted (List ll, List [Val rh])
wellSorted (Val lh, Val rh) = compare rh lh
wellSorted (List (_:_), List []) = LT
wellSorted (List [], List (_:_)) = GT
wellSorted (List [], List []) = EQ

main :: IO ()
main = do
    inputLines <- lines <$> getContents
    let Right pairs = parsePairs inputLines
    let sep1 = List [List [Val 2]]
    let sep2 = List [List [Val 6]]
    let packets = sortBy (\a b -> wellSorted (b, a)) $ ([sep1, sep2] ++) $ concat $ (\(x,y) -> [x,y]) <$> pairs
    print $ ((fromJust $ (1+) <$> elemIndex sep1 packets) * (fromJust $ (1+) <$> elemIndex sep2 packets))