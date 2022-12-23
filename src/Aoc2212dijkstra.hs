module Aoc2212dijkstra (aoc2212dijkstra) where

import Lib
import qualified Data.List.Extra as List
import qualified Data.Char as Char

-- Part a -- Solve Day 12 less naively using Dijkstra's algorithm https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm

type Coords = (Int , Int)
type Source = Coords
type Target = Coords
-- origin: upper left
type HeightField = [[Char]]
type DistanceField = [[Int]]
type PreviousNodeField = [[Coords]]
type UnvisitedQueue = [Coords]

data State = State HeightField DistanceField PreviousNodeField UnvisitedQueue Target
instance Show State
    where show (State hf df pnf uq t) = let hfStr = "Height Field:\n-------------\n" ++ join "\n" hf
                                            dfStr = "\n\nDistance Field:\n---------------\n" ++ toPrettyField df infinity "∞"
                                            pnfStr = "\n\nPrevious Node Field:\n--------------------\n" ++ toPrettyField pnf undef "⊥"
                                            uqStr = "\n\nNodes not visited, yet:\n-----------------------\n" ++ show uq
                                            tStr = "\n\nTarget:\n-------\n" ++ show t ++ " has distance: " ++ replaceSymbol infinity "∞" (show (valAt df t))
                                        in hfStr ++ dfStr ++ pnfStr ++ uqStr ++ tStr
                                    
toPrettyField :: Show a => [[a]] -> a -> String -> String
toPrettyField fld val symbol = join "\n" (map (join "\t" . map (replaceSymbol val symbol . show)) fld)

replaceSymbol :: Show a => a -> String -> String -> String
replaceSymbol val symbol s = if s == show val then symbol else s

undef :: Coords
undef = (minBound :: Int, minBound :: Int) -- any value not reachable on any field

infinity :: Int
infinity = maxBound :: Int

type Dir = Coords
up :: Dir
up = (0, -1)
down :: Dir
down = (0, 1)
left :: Dir
left = (-1 , 0)
right :: Dir
right = (1, 0)

add :: Coords -> Coords -> Coords
add (x, y) (a, b) = (x + a, y + b)

valAt :: [[a]] -> Coords -> a
valAt ls (x,y) = (ls !! y) !! x

setAt :: [a] -> Int -> a -> [a]
setAt xs idx x = take idx xs ++ [x] ++ drop (idx + 1) xs

replaceValAt :: Coords -> a -> [[a]] -> [[a]]
replaceValAt  _ _ [] = []
replaceValAt (x, 0) val (xs:xss) = setAt xs x val : xss
replaceValAt (x, y) val (xs:xss) = xs : replaceValAt (x, y - 1) val xss

bounds :: [[a]] -> ((Int, Int),(Int, Int))
bounds [] = ((0, 0), (0, 0))
bounds (ls:lss) = ((0, 0), (length ls, length (ls:lss)))

stepInValidDirs :: HeightField -> Coords -> [Coords]
stepInValidDirs fld pos = let neighbors = map (add pos) [up, down, left, right]
                              inBounds = filter (isInBounds fld) neighbors
                              reachables = filter (reachable fld pos) inBounds
                          in reachables

reachable :: HeightField -> Coords -> Coords -> Bool
reachable fld cur next = let curVal = Char.ord $ valAt fld cur
                             nextVal = Char.ord $ valAt fld next
                         in (curVal + 1) >= nextVal

isInBounds :: HeightField -> Coords -> Bool
isInBounds fld (x,y) = let ((lowX, lowY), (highX, highY)) = bounds fld
                       in x >= lowX && x < highX && y >= lowY && y < highY

initField :: [[a]] -> b -> [[b]]
initField lss e = map (map (const e)) lss

toCoordList :: [[a]] -> [Coords]
toCoordList lss = let (_, (x, y)) = bounds lss
                      xss = replicate y $ take x [0..]
                      ys = take y [0..]
                      coordField = zipWith (\xs y' -> map (fixSnd y') xs) xss ys
                  in  concat coordField

initialize :: (HeightField, Source, Target) -> State
initialize (hf, s, t) = let df = replaceValAt s 0 $ initField hf infinity
                            pnf = initField hf undef
                            uq = toCoordList hf
                        in State hf df pnf uq t

step :: State -> State
step (State hf df pnf [] t) = State hf df pnf [] t
step (State hf df pnf uq t) = let sortedQueue = List.sortBy (\x y -> compare (valAt df x) (valAt df y)) uq
                                  u = head sortedQueue
                                  newUq = tail sortedQueue
                                  neighbors = stepInValidDirs hf u
                                  (newDf, newPnf) = updateDistances u neighbors df pnf
                              in if valAt df u == infinity then State hf df pnf [] t else State hf newDf newPnf newUq t

updateDistances :: Coords -> [Coords] -> DistanceField -> PreviousNodeField -> (DistanceField, PreviousNodeField)
updateDistances _ [] df pnf = (df, pnf)
updateDistances u (v:vs) df pnf = let (newDf, newPnf) = updateDistance u v df pnf
                                  in updateDistances u vs newDf newPnf

updateDistance :: Coords -> Coords -> DistanceField -> PreviousNodeField -> (DistanceField, PreviousNodeField)
updateDistance u v df pnf = let alt = 1 + valAt df u
                                distV = valAt df v
                                smaller = alt < distV
                                newDf = if smaller then replaceValAt v alt df else df
                                newPnf = if smaller then replaceValAt v u pnf else pnf
                            in (newDf, newPnf)

whileDo :: (a -> Bool) -> (a -> a) -> a -> a
whileDo condition f x
    | condition x = x
    | otherwise = whileDo condition f (f x)

isFinished :: State -> Bool
isFinished (State _ _ _ [] _) = True
isFinished (State _ df _ _ t) = valAt df t /= infinity

dijkstra :: (HeightField, Source, Target) -> State
dijkstra = whileDo isFinished step . initialize 

shortestPath :: (HeightField, Source, Target) -> Int
shortestPath input = let (State _ df _ _ t) = dijkstra input
                     in valAt df t

solveA :: (HeightField, Source, Target) -> Int
solveA = shortestPath

-- Part b

getValleyCoords :: HeightField -> [Coords]
getValleyCoords hf = let ((minX, minY), (maxX, maxY)) = bounds hf
                         coordsInBounds = [(x, y) | x <- [minX .. (maxX -1)], y <- [minY .. (maxY -1)]]
                    in filter (\pos -> valAt hf pos == 'a') coordsInBounds

solveB :: (HeightField, Source, Target) -> Int
solveB (hf, _, t) = let lowestPoints = getValleyCoords hf
                        allShortestPaths = map (\pos -> shortestPath (hf, pos, t)) lowestPoints
                    in minimum allShortestPaths

---

aoc2212dijkstra :: IO ()
aoc2212dijkstra = do 
    input <- readFile "./resources/input12test"
    -- input <- readFile "./resources/input12real"
    -- part a --- result: 391, CPU time: 2.78s
    -- print $ dijkstra $ translate $ splitIntoLines input
    print $ solveA $ translate $ splitIntoLines input
    -- part b --- just brute force it --- result: 386, CPU time: 578.40s
    print $ solveB $ translate $ splitIntoLines input

translate:: [[Char]] -> (HeightField, Source, Target)
translate chrs = let replaceLine = map (\c -> if c == 'S' then 'a' else (if c == 'E' then 'z' else c))
                     field = map replaceLine chrs
                     start = fieldElemIndex 'S' chrs
                     dest = fieldElemIndex 'E' chrs
                 in (field, start, dest)

fieldElemIndex :: Eq a => a -> [[a]] -> (Int, Int)
fieldElemIndex e fld = let linearIdx = indexOf e $ concat fld
                           (_,(bound,_)) = bounds fld
                           toCoords x = (x `mod` bound, x `div` bound)
                       in toCoords linearIdx