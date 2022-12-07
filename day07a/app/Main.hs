{-# LANGUAGE RankNTypes #-}

module Main where

import qualified Data.Map as M
import Data.Function ( on )

join :: [a] -> [[a]] -> [a]
join p (h:[]) = h
join p (h:t) = h ++ p ++ join p t
join p [] = []

splitPred:: (a -> Bool) -> [a] -> [[a]]
splitPred _ [] = []
splitPred pred items = do
    let (h, t) = break pred items
    [h] ++ splitPred pred (drop 1 t)

splitSep:: forall a. Eq a => a -> [a] -> [[a]]
splitSep sep items = splitPred (sep ==) items

resolvePath :: [String] -> [String] -> [String]
resolvePath cwd (".." : path) = init cwd
resolvePath cwd (segment : path) = cwd ++ [segment]
resolvePath cwd [] = cwd

allSplits :: [String] -> [String] -> [([String], [String])]
allSplits [] suffix = [([], suffix)]
allSplits prefix suffix = [(prefix, suffix)] ++ allSplits (init prefix) ([last prefix] ++ suffix)

search :: [String] -> [String] -> [([String], [String], Integer)]
search cwd (('$' : ' ' : 'c' : 'd' : ' ' : '/' : []):rest) = search [] rest
search cwd (('$' : ' ' : 'c' : 'd' : ' ' : path):rest) = search (resolvePath cwd (splitSep '/' path)) rest
search cwd (('$' : ' ' : 'l' : 's' : []):rest) = search cwd rest
search cwd (file : rest) = do
    let (fileType : name : []) = splitSep ' ' file
    case fileType of
        "dir" -> search cwd rest
        fSize -> do
            let size = read fSize :: Integer
            ((\(pref, suf) -> (pref, suf, size)) <$> (allSplits cwd [name])) ++ search cwd rest
search cwd [] = []

main :: IO ()
main = do
    input <- lines <$> getContents 
    let fstOf3 = (\(a, b, c) -> a)
    let files = search [] input
    let folders = M.fromListWith (++) [(k, [v]) | v@(k, _, _) <- files]
    print (sum . (filter (<= 100000)) $ (sum <$> (((\(_, _, v) -> v) <$>) <$> M.elems folders)))