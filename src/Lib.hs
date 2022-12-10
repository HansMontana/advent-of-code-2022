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
    mapTuple,
    flatten,
    mapWithIndex,
    printList,
    unique,
    times,
    join,
    foldlList
    ) where

import qualified Data.List.Split as Split
import qualified Data.Set as Set

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

-- day 8

flatten :: [[a]] -> [a]
flatten = foldr1 (++)

mapWithIndex :: (a -> Int -> b) -> [a] -> [b]
mapWithIndex f ls = zipWith f ls [0..]

-- day 9

printList :: Show a => [a] -> IO()
printList = mapM_ print

unique :: Ord a => [a] -> Int
unique = Set.size . Set.fromList

times :: Int -> (a -> a) -> a -> a 
times 0 _ x = x
times n f x = times (n-1) f (f x)  

-- day 7

join :: [a] -> [[a]] -> [a]
join delimiter = foldr1 (\x y -> x ++ delimiter ++ y)

-- day 10

foldlList :: (b -> a -> b) -> b -> [a] -> [b]
foldlList f start= reverse . foldlList' f [start]
    where foldlList' :: (b -> a -> b) -> [b] -> [a] -> [b]
          foldlList' f ys [] = ys
          foldlList' f (y:ys) (x:xs) = let result = f y x
                                           results = result:y:ys
                                       in foldlList' f results xs