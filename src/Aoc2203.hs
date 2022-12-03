module Aoc2203 (aoc2203) where

import Lib
import qualified Data.List as List
import qualified Data.Char as Char

-- Part a

-- no special handling for uneven length
toCompartment :: [Char] -> ([Char], [Char])
toCompartment ls = let compartmentSize = div (length ls) 2
                   in splitAt compartmentSize ls

inBoth :: ([Char], [Char]) -> Char
inBoth (xs, ys) = head $ List.intersect xs ys

toPriority :: Char -> Int
toPriority c = Char.ord c - (if Char.isUpper c then 38 else 96)

-- part b

-- no sensible handling of n % 3 /= 0 except for just dropping trailing elements
toGroups :: [[Char]] ->[([Char], [Char] , [Char])]
toGroups [] = []
toGroups [_] = []
toGroups [_, _] = []
toGroups (x:y:z:ls) = (x,y,z):toGroups ls

inThreeoth :: ([Char], [Char], [Char]) -> Char
inThreeoth (xs, ys, zs) = head $ foldr1 List.intersect [xs, ys, zs]

aoc2203 :: IO ()
aoc2203 = do 
    -- input <- readFile "./resources/input3test"
    input <- readFile "./resources/input3real"
    -- part a
    print $ sum $ map (toPriority . inBoth . toCompartment) $ splitIntoLines input
    -- part b
    print $ sum $ map (toPriority . inThreeoth) $ toGroups $ splitIntoLines input



