{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.List (mapAccumL, sort)
import Data.Bits
import Debug.Trace (trace)

data Hasher = Divisible Integer deriving (Show)

data Operator = Mul | Add deriving (Show)
data Operand = Old | ConstVal Integer deriving (Show)
data Op = Op {
    operator :: Operator,
    a :: Operand,
    b :: Operand
} deriving (Show)

data Monkey = Monkey {
    op :: Op,
    hasher :: Hasher,
    succTgt :: Int,
    failTgt :: Int
} deriving (Show)

chooseOperator :: Num a => Operator -> a -> a -> a
chooseOperator Add = (+)
chooseOperator Mul = (*)

evalExpr :: Integer -> Operand -> Integer
evalExpr old Old = old
evalExpr old (ConstVal a) = a

eval :: Integer -> Op -> Integer
eval old (Op (chooseOperator -> op) (evalExpr old -> a) (evalExpr old -> b)) = a `op` b

mkOperand :: String -> Operand
mkOperand "old" = Old
mkOperand (read -> val) = ConstVal val

mkOperator :: String -> Operator
mkOperator "*" = Mul
mkOperator "+" = Add

mkHasher :: [String] -> Hasher
mkHasher ["divisible", "by", read -> constValue] = Divisible constValue

applyHasher :: Hasher -> Integer -> Integer
applyHasher (Divisible mo) val = val `mod` mo

startingVals :: [String] -> [Integer]
startingVals (x : xs) = (read $ filter (/= ',') x) : startingVals xs
startingVals [] = []

parseMonkey :: [[String]] -> (Monkey, [Integer])
parseMonkey [
    ["Monkey", _ ],
    ("Starting" : "items:" : (startingVals -> startWorr)),
    ["Operation:", "new", "=", (mkOperand -> op1), (mkOperator -> operator), (mkOperand -> op2)],
    ("Test:" : (mkHasher -> hasher)),
    ["If", "true:", "throw", "to", "monkey", read -> monkeyTgtSucc],
    ["If", "false:", "throw", "to", "monkey", read -> monkeyTgtFail],
    []] = (Monkey (Op operator op1 op2) hasher monkeyTgtSucc monkeyTgtFail, startWorr)

parseMonkeyAll :: [[String]] -> [(Monkey, [Integer])]
parseMonkeyAll [] = []
parseMonkeyAll ls = (parseMonkey $ take 7 ls) : (parseMonkeyAll $ drop 7 ls)

doThrow :: Int -> Int -> Integer -> [(Int, [Integer])] -> [(Int, [Integer])]
doThrow from to item input = [ 
    if from == monkeyPos
        then (throwCnt + 1, [])
    else if to == monkeyPos
        then (throwCnt, items ++ [item])
    else (throwCnt, items)
    | (monkeyPos, (throwCnt, items)) <- zip [0..] input]

doThrowTurn :: Monkey -> Int -> [Integer] -> [(Int, [Integer])] -> [(Int, [Integer])]
doThrowTurn (Monkey _ hasher succTgt failTgt) from items input = do
    foldl (\acc worr -> doThrow from (if applyHasher hasher worr == 0 then succTgt else failTgt) worr acc) input items

inspect :: Integer -> Monkey -> [Integer] -> [Integer]
inspect commonModulo (Monkey op hasher _ _) items = [mod (eval item op) commonModulo | item <- items]

procIter :: [Monkey] -> [(Int, [Integer])] -> [(Int, [Integer])]
procIter monkeys vals = foldl (\acc (monkeyPos, monkey) -> doThrowTurn monkey monkeyPos (inspect (product [ a | Monkey _ (Divisible a) _ _ <- monkeys]) monkey $ snd (acc !! monkeyPos)) acc) vals (zip [0..] monkeys)

main :: IO ()
main = do
    inputLines <- (words <$>) <$> lines <$> getContents
    let parsed = (parseMonkeyAll inputLines)
    let monkeys = fst <$> parsed
    let vals = snd <$> parsed
    let mapper = procIter monkeys
    print . product . take 2 . reverse . sort $ fst <$> foldl (\acc _ -> mapper acc) ((\x -> (0, x)) <$> vals) [1..10000]