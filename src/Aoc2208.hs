module Aoc2208 (aoc2208) where

import Lib
import qualified Data.List.Extra as List

-- Some type aliases and methods for them used in both part a and b.

type Field = [[Int]]
type Views = (Field, Field, Field, Field) -- left, right, top, bottom
type VisibleField = [[Bool]]
type VisibleViews = (VisibleField, VisibleField, VisibleField, VisibleField)
type Line = [Int]
type VisibleLine = [Bool]

toViews :: Field -> Views
toViews fld = let transposed = List.transpose fld
              in (fld, map reverse fld, transposed, map reverse transposed)

mapView :: (Field -> [[a]]) -> Views -> ([[a]],[[a]],[[a]],[[a]])
mapView f (l, r, t, b) = (f l, f r, f t, f b)

-- Part a

visibleFromLeftAtPos :: Line -> Int -> Bool
visibleFromLeftAtPos ls pos = let (preElems, _) = List.splitAt pos ls
                                  el = ls !! pos
                                  maxPreElem = if null preElems then minBound :: Int else maximum preElems
                              in maxPreElem < el

visibleFromLeftForLine :: Line -> VisibleLine
visibleFromLeftForLine ls = mapWithIndex visibleFromLeftAtPos $ replicate (length ls) ls

visibleFromLeft :: Field -> VisibleField
visibleFromLeft = map visibleFromLeftForLine

visibleFromLeftForViews :: Views -> VisibleViews
visibleFromLeftForViews = mapView visibleFromLeft

-- The resulting View is kind of against the implicit conventional use of Views so far
-- as they are all from the same edge now
toLeftViewsOnly :: ([[a]],[[a]],[[a]],[[a]]) -> ([[a]],[[a]],[[a]],[[a]])
toLeftViewsOnly (l, r, t, b) = (l, map reverse r, List.transpose t, List.transpose $ map reverse b)

combineFields :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
combineFields _ _ [] = [] 
combineFields _ [] _ = [] 
combineFields f (xs:xss) (ys:yss) = let combined = zipWith f xs ys
                                    in combined : combineFields f xss yss

combineViews :: (a -> a -> a) -> ([[a]],[[a]],[[a]],[[a]]) -> [[a]]
combineViews f (l, r, t, b) = combineFields f l $ combineFields f r $ combineFields f t b

visibleTrees :: VisibleViews -> VisibleField
visibleTrees = combineViews (||)  

sumVisible :: VisibleField -> Int
sumVisible fld = length $ filter id $ flatten fld

solveA :: String -> Int
solveA input = sumVisible $ visibleTrees $ toLeftViewsOnly $ visibleFromLeftForViews $ toViews $ toIntField $ splitIntoLines input

-- part b 
-- Could have been based more on part a if I've used generic typing and some params
-- because the structure of most methods is similar if not the same.

scoreToLeftAtPos :: Line -> Int -> Int
scoreToLeftAtPos ls pos = scoreLine (ls!!pos) $ reverse $ fst $ List.splitAt pos ls

scoreLine :: Int -> [Int] -> Int
scoreLine _ [] = 0
scoreLine h (t:ts) = 1 + (if t >= h then 0 else scoreLine h ts)

scoreToLeftForLine :: Line -> Line
scoreToLeftForLine ls = mapWithIndex scoreToLeftAtPos $ replicate (length ls) ls

scoreToLeft :: Field -> Field
scoreToLeft = map scoreToLeftForLine

scoreToLeftForViews :: Views -> Views
scoreToLeftForViews = mapView scoreToLeft 

score :: Views -> Field
score = combineViews (*)  

maxOfField :: Field -> Int
maxOfField fld = maximum $ flatten fld

solveB :: String -> Int
solveB input = maxOfField $ score $ toLeftViewsOnly $ scoreToLeftForViews $ toViews $ toIntField $ splitIntoLines input

-- "main" for this day

aoc2208 :: IO ()
aoc2208 = do 
    -- input <- readFile "./resources/input8test"
    input <- readFile "./resources/input8real"
    -- part a: 1843
    print $ solveA input
    -- part b: 180000
    print $ solveB input

-- For basic input conversion

toIntField :: [[Char]] -> [[Int]]
toIntField = map (map (readInt.(: [])))

-- Some pretty printing functions (at the moment with unused warning, as they were used for debugging).
-- Maybe it's time to get into unit testing in haskell.

{- printField :: Show a => [[a]] -> IO()
printField = mapM_ print

printViews :: Show a => ([[a]],[[a]],[[a]],[[a]]) -> IO()
printViews (l,r,t,b) = do
    printField l
    putStrLn ""
    printField r
    putStrLn ""
    printField t
    putStrLn ""
    printField b -}