{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

main :: IO ()
main = do
    inputLines <- lines <$> getContents
    print inputLines