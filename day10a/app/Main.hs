{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.List ()
import qualified Data.Map as M

data Instruction = Noop | AddXExec | AddXWriteBack Integer deriving(Show);

mkInstr :: [String] -> [Instruction]
mkInstr ["noop"] = [Noop]
mkInstr ["addx", read -> a] = [AddXExec, AddXWriteBack a]

newtype InstPtr = InstPtrVal Integer
newtype Signal = SignalVal Integer

evalInstr :: (InstPtr, Signal, Integer) -> Instruction -> (InstPtr, Signal, Integer)
evalInstr (InstPtrVal ptr, SignalVal signal, sigSum) Noop = (
        InstPtrVal $ ptr + 1, 
        SignalVal signal, 
        if (ptr + 20) `mod` 40 == 0 then sigSum + ptr * signal else sigSum
    )
evalInstr (InstPtrVal ptr, SignalVal signal, sigSum) AddXExec = (
        InstPtrVal $ ptr + 1, 
        SignalVal signal,
        if (ptr + 20) `mod` 40 == 0 then sigSum + ptr * signal else sigSum
    )
evalInstr (InstPtrVal ptr, SignalVal signal, sigSum) (AddXWriteBack val) = (
        InstPtrVal $ ptr + 1,
        SignalVal $ signal + val,
        if (ptr + 20) `mod` 40 == 0 then sigSum + ptr * signal else sigSum
    )

main :: IO ()
main = do
    inputLines <- lines <$> getContents
    print $ (\(_, _, sum) -> sum) $ foldl evalInstr (InstPtrVal 1, SignalVal 1, 0) $ foldl (++) [] $ mkInstr <$> words <$> inputLines