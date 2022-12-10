module Aoc2211A (aoc2211a) where

import Lib
import qualified Data.List.Extra as List

type Item = Int
type Inspect = Item -> Item
type Test = Item -> Item
type Handled = Int

data Monkey = Monkey [Item] Inspect Test Handled
instance Show Monkey where
    show (Monkey items _ _ handled) = show (items, handled)

reliefOp :: Item -> Item
reliefOp = flip div 3

numOfRounds :: Int
numOfRounds = 20

toTimesHandled :: Monkey -> Handled
toTimesHandled (Monkey _ _ _ handled) = handled

determineMonkeyBusiness :: [Monkey] -> Int
determineMonkeyBusiness ms = product $ take 2 $ reverse $ List.sort $ map toTimesHandled ms

translateItems :: String -> [Item]
translateItems s = map readInt $ List.splitOn ", " $ drop 18 s

translateInspect :: String -> Inspect
translateInspect s = let (op:operand:_) = List.splitOn " " $ drop 23 s
                         operation = if op == "+" then (+) else (*)
                     in if operand == "old" then (\x -> operation x x) else operation $ readInt operand

divisibleBy :: Int -> Int -> Bool
divisibleBy divisor x = (x `mod` divisor) == 0

translateTest:: String -> String -> String -> Test
translateTest condLine trueLine falseLine = let boolTest = divisibleBy $ readInt $ drop 21 condLine
                                                trueCase = readInt $ drop 29 trueLine
                                                falseCase = readInt $ drop 30 falseLine
                                            in (\x -> if boolTest x then trueCase else falseCase)

transLateMonkey :: [String] -> Monkey
transLateMonkey (_:itemLine:inspectLine:testLine:trueLine:falseLine:_) = Monkey (translateItems itemLine) 
                                                                                (translateInspect inspectLine)
                                                                                (translateTest testLine trueLine falseLine)
                                                                                0

transLateMonkeys :: [[String]] -> [Monkey]
transLateMonkeys = map transLateMonkey

step :: [Monkey] -> Int -> [Monkey]
step ms curIndex = let (Monkey items inspect test handled) = ms !! curIndex
                       itemlessMonkey = Monkey [] inspect test (handled + length items)
                       (before, _:after) = List.splitAt curIndex ms
                       theNewMonkeys = before ++ [itemlessMonkey] ++ after
                    in distributeItems theNewMonkeys items inspect test 

distributeItems :: [Monkey] -> [Item] -> Inspect -> Test -> [Monkey]
distributeItems ms [] _ _ = ms
distributeItems ms (i:items) insp test = distributeItems (distributeItem ms i insp test) items insp test

distributeItem :: [Monkey] -> Item -> Inspect -> Test -> [Monkey]
distributeItem ms item insp test = let itemAfterInspection = reliefOp $ insp item
                                       nextMonkeyIndex = test itemAfterInspection
                                   in giveToMonkey ms nextMonkeyIndex itemAfterInspection

giveToMonkey :: [Monkey] -> Int -> Item -> [Monkey]
giveToMonkey ms monkeyIndex item = let (Monkey items inspect test handled) = ms !! monkeyIndex
                                       itemfulMonkey = Monkey (items++[item]) inspect test handled
                                       (before, _:after) = List.splitAt monkeyIndex ms
                                   in before ++ [itemfulMonkey] ++ after

doRound :: [Monkey] -> [Monkey]
doRound ms = doRound' ms [0..(length ms -1)]
    where doRound' :: [Monkey] -> [Int] -> [Monkey]
          doRound' mks [] = mks
          doRound' mks (i:indices) = doRound' (step mks i) indices

doRounds :: [Monkey] -> Int -> [Monkey]
doRounds ms n = times n doRound ms

aoc2211a :: IO ()
aoc2211a = do 
    -- input <- readFile "./resources/input11test"
    input <- readFile "./resources/input11real"
    print $ determineMonkeyBusiness $ flip doRounds numOfRounds $ transLateMonkeys $ readInput input

readInput :: String -> [[String]]
readInput = map splitIntoLines . splitIntoBlocks