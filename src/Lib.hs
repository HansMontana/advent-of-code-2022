module Lib (
    readInt,
    splitIntoInts,
    splitIntoLines,
    splitIntoBlocks,
    splitIntoListsOfInt,
    splitIntoWords,
    toTuples,
    toTuple,
    toTuplesOfTuples,
    splitIntoPairs,
    splitTupleIntoPairOfPairs,
    mapTuple
    ) where

import qualified Data.List.Split as Split

-- Warning: pattern matching in this lib is not always exhaustive.
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
toTuple [_] = ("","")
toTuple (a:b:_) = (a,b)

toTuples :: String -> [(String, String)]
toTuples s = map (toTuple . splitIntoWords) $ splitIntoLines s

-- day 4

toTuplesOfTuples :: String -> [((String, String),(String , String))]
toTuplesOfTuples input = map (splitTupleIntoPairOfPairs . splitIntoPairs ",") (splitIntoLines input)

splitIntoPairs :: String  -> String -> (String, String)
splitIntoPairs delimiter s = let [x, y] = take 2 $ Split.splitOn delimiter s
                             in (x,y)

splitTupleIntoPairOfPairs :: (String, String) -> ((String, String),(String, String))
splitTupleIntoPairOfPairs (x, y) = (splitIntoPairs "-" x, splitIntoPairs "-" y)

mapTuple:: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)