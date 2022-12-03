module Lib ( 
    splitIntoInts,
    splitIntoLines,
    splitIntoBlocks,
    splitIntoListsOfInt,
    splitIntoWords,
    toTuples,
    toTuple,
    readInt
    ) where

import qualified Data.List.Split as Split

-- day 1

splitIntoInts :: String -> [Int]
splitIntoInts s = map readInt $ splitIntoLines s

splitIntoLines :: String -> [String]
splitIntoLines = Split.splitOn "\n"

splitIntoBlocks :: String -> [String]
splitIntoBlocks = Split.splitOn "\n\n"

splitIntoListsOfInt :: String -> [[Int]]
splitIntoListsOfInt s = map splitIntoInts $ splitIntoBlocks s

readInt :: String -> Int
readInt = read

-- day 2

splitIntoWords :: String -> [String]
splitIntoWords = Split.splitOn " "

toTuple::[String] -> (String,String)
toTuple [] = ("","")
toTuple [a] = ("","")
toTuple (a:b:_) = (a,b)

toTuples :: String -> [(String, String)]
toTuples s = map (toTuple . splitIntoWords) $ splitIntoLines s
