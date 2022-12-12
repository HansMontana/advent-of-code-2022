module Aoc2213 (aoc2213) where

import Lib
import qualified Data.List.Extra as List
import qualified Data.List.Split as Split
import qualified Data.Maybe as Maybe

-- Part a

data Ternary = Yes | No | Unknown deriving (Eq, Show)

toBool :: Ternary -> Bool
toBool Yes = True
toBool No = False
toBool Unknown = True

fromBool :: Bool -> Ternary
fromBool True = Yes
fromBool False = No

compareSignals :: [[String]] -> [Bool]
compareSignals = map compareSignal

compareSignal :: [String] -> Bool
compareSignal (s1:s2:_) = toBool $ compareVals s1 s2

compareLists :: [String] -> [String] -> Ternary
compareLists [] [] = Unknown 
compareLists [] _ = Yes
compareLists _ [] = No
compareLists (s1:s1s) (s2:s2s) = case compareVals s1 s2 of
                                      Unknown -> compareLists s1s s2s
                                      Yes -> Yes
                                      No -> No

compareVals :: String -> String -> Ternary
compareVals s1 s2 = case (isNum s1, isNum s2) of
                         (True, True) -> if readInt s1 == readInt s2 then Unknown else fromBool (readInt s1 <= readInt s2)
                         (True, False) -> compareLists [s1] (toList s2)
                         (False, True) -> compareLists (toList s1) [s2]
                         (False, False) -> compareLists (toList s1) (toList s2)

isNum :: String -> Bool
isNum s = head s /= '['

toList :: String -> [String]
toList s = let unboxed = unbox s
           in if "" == unboxed then []
                                 else if isBracketedList unboxed then [unboxed]
                                                                 else splitIntoElems unboxed

-- Might want to get into regex libs for haskell, but I don't know if any of them support context free languages
splitIntoElems :: String -> [String]
splitIntoElems = Split.splitOn "$" . splitIntoElems' 0
    where splitIntoElems' :: Int -> String -> String
          splitIntoElems' _ "" = []
          splitIntoElems' acc ('[':s) = '[' : splitIntoElems' (acc+1) s
          splitIntoElems' acc (']':s) = ']' : splitIntoElems' (acc-1) s
          splitIntoElems' acc (',':s) = if acc == 0 then '$' : splitIntoElems' acc s else ',' : splitIntoElems' acc s
          splitIntoElems' acc (c:s) = c : splitIntoElems' acc s

unbox :: String -> String
unbox s = (drop 1 . take (length s - 1)) s

isBracketedList :: String -> Bool
isBracketedList str = head str == '[' && last str == ']' && isBracketedList' str
    where isBracketedList' s = let innerBrackets = unbox $ filter (\c -> c == '[' || c == ']') s
                               in noCloseWithoutOpen 0 innerBrackets 

noCloseWithoutOpen :: Int -> String -> Bool
noCloseWithoutOpen n [] = n == 0
noCloseWithoutOpen n (b:bs) = case b of
                                   '[' -> noCloseWithoutOpen (n + 1) bs
                                   ']' -> n >= 1 && noCloseWithoutOpen (n - 1) bs

solveA :: [[String]] -> Int
solveA input = sum $ map fst $ filter snd $ zip [1..] $ compareSignals input

-- Part b

dividers :: [String]
dividers = ["[[2]]","[[6]]"]

compareFun :: String -> String -> Ordering
compareFun s1 s2 = toOrdering $ compareVals s1 s2

toOrdering :: Ternary -> Ordering
toOrdering Yes = LT
toOrdering No = GT
toOrdering Unknown = EQ

solveB :: [[String]] -> Int
solveB input = let sorted = List.sortBy compareFun $ flatten input ++ dividers
                   dividersIdx = map (\e -> (+ 1) $ indexOf e sorted) dividers
               in product dividersIdx

-- "main"

aoc2213 :: IO ()
aoc2213 = do 
    -- input <- readFile "./resources/input13test"
    input <- readFile "./resources/input13real"
    putStrLn "Part a:"
    print $ solveA $ translate input
    putStrLn "\nPart b:"
    print $ solveB $ translate input

-- Convert initial input

translate :: String -> [[String]] 
translate =  map splitIntoLines . splitIntoBlocks