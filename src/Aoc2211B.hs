module Aoc2211B (aoc2211b) where

import Lib
import qualified Data.List.Extra as List

type Item = Int
type Inspect = Item -> Item
type Divisor = Int
type Test = (Divisor, Item -> Item)
type Handled = Int

data Monkey = Monkey [Item] Inspect Test Handled
instance Show Monkey where
    show (Monkey items _ (divisor, _) handled) = show (items, handled, divisor)

-- Even though we don't feel relief anymore, we can build our own function using the lcm https://en.wikipedia.org/wiki/Least_common_multiple
-- of all divisors used by the monkeys to check where to toss the items to. Somehow these divisors are all primes, so we can just use
-- the product of the set of them.
reliefOp :: [Monkey] -> Item -> Item
reliefOp ms item = mod item $ toLcm ms

toLcm :: [Monkey] -> Int
toLcm = product . removeDuplicates . map getDivisor

getDivisor :: Monkey -> Divisor
getDivisor (Monkey _ _ (divisor, _) _) = divisor

numOfRounds :: Int
numOfRounds = 10000

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
translateTest condLine trueLine falseLine = let divisor = readInt $ drop 21 condLine
                                                boolTest = divisibleBy divisor
                                                trueCase = readInt $ drop 29 trueLine
                                                falseCase = readInt $ drop 30 falseLine
                                            in (divisor, \x -> if boolTest x then trueCase else falseCase)

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
distributeItem ms item insp test = let itemAfterInspection = reliefOp ms $ insp item
                                       nextMonkeyIndex = snd test itemAfterInspection
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

aoc2211b :: IO ()
aoc2211b = do 
    -- input <- readFile "./resources/input11test"
    input <- readFile "./resources/input11real"
    print $ determineMonkeyBusiness $ flip doRounds numOfRounds $ transLateMonkeys $ readInput input

readInput :: String -> [[String]]
readInput = map splitIntoLines . splitIntoBlocks