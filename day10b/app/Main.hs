{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.List (intercalate)

data Instruction = Noop | AddXExec | AddXWriteBack Integer deriving(Show);

mkInstr :: [String] -> [Instruction]
mkInstr ["noop"] = [Noop]
mkInstr ["addx", read -> a] = [AddXExec, AddXWriteBack a]

newtype InstPtr = InstPtrVal Integer
newtype Signal = SignalVal Integer

scanlinePos :: Integer -> Integer
scanlinePos idx = idx `mod` 40

evalInstr :: (InstPtr, Signal, [Char]) -> Instruction -> (InstPtr, Signal, [Char])
evalInstr (InstPtrVal ptr, SignalVal signal, lst) Noop = (
        InstPtrVal $ ptr + 1, 
        SignalVal signal, 
        lst ++ (if abs (signal - (scanlinePos ptr)) <= 1 then "#" else ".")
    )
evalInstr (InstPtrVal ptr, SignalVal signal, lst) AddXExec = (
        InstPtrVal $ ptr + 1, 
        SignalVal signal,
        lst ++ (if abs (signal - (scanlinePos ptr)) <= 1 then "#" else ".")
    )
evalInstr (InstPtrVal ptr, SignalVal signal, lst) (AddXWriteBack val) = (
        InstPtrVal $ ptr + 1,
        SignalVal $ signal + val,
        lst ++ (if abs (signal - (scanlinePos ptr)) <= 1 then "#" else ".")
    )

chunks :: [a] -> [[a]]
chunks [] = []
chunks xs = take 40 xs : (chunks $ drop 40 xs)

main :: IO ()
main = do
    inputLines <- lines <$> getContents
    mapM_ putStrLn (chunks $ (\(_, _, screen) -> screen) $ foldl evalInstr (InstPtrVal 0, SignalVal 1, "") $ foldl (++) [] $ mkInstr <$> words <$> inputLines)