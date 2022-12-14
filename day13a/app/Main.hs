{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Language 
import qualified Text.ParserCombinators.Parsec.Token as P
import Control.Arrow 
import Data.Functor.Identity
import Text.Parsec.Prim

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser emptyDef

decimal :: ParsecT String u Identity Integer
decimal = P.integer lexer

commaSep :: ParsecT String u Identity a -> ParsecT String u Identity [a]
commaSep = P.commaSep lexer

data Item = List [Item] | Val Integer deriving Show
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

countPairs :: [ItemPair] -> [Int]
countPairs pairs = [if ws == GT then i else 0 | (wellSorted -> ws, i) <- zip pairs [1..] ]

main :: IO ()
main = do
    inputLines <- lines <$> getContents
    let Right pairs = parsePairs inputLines
    print . sum $ countPairs pairs