import System.IO
import System.Environment
import Data.Char
import Data.List

main = do
    contents <- readFile "input.txt"
    args <- getArgs
    let monkeys = parseMonkeys $ lines contents
        n = read $ head args
    print $ part1 monkeys n


part1 :: [Monkey] -> Int -> Int
part1 monkeys n = monkeyBusiness $ runRounds n monkeys


type Monkey = ([Int], Int -> Int, Int -> Bool, (Int, Int), Int)

getItems :: Monkey -> [Int]
getItems (items, _, _, _, _) = items

getOperation :: Monkey -> (Int -> Int)
getOperation (_, operation, _, _, _) = operation

getTest :: Monkey -> (Int -> Bool)
getTest (_, _, test, _, _) = test

getReceivers :: Monkey -> (Int, Int)
getReceivers (_, _, _, receivers, _) = receivers

getItemsInspected :: Monkey -> Int
getItemsInspected (_, _, _, _, itemsInspected) = itemsInspected



parseMonkeys :: [String] -> [Monkey]
parseMonkeys = map parseMonkey . splitOn ""

parseMonkey :: [String] -> Monkey
parseMonkey input = (items, operation, test, receivers, 0)
    where items = map parseInt $ drop 2 $ words $ input !! 1
          operation = parseOperation $ input !! 2
          test = parseTest $ input !! 3
          receivers = (digitToInt $ last $ input !! 4, digitToInt $ last $ input !! 5)

parseInt :: String -> Int
parseInt = read . takeWhile isDigit

parseOperation :: String -> (Int -> Int)
parseOperation s = (\x -> operator (parseOperand x (ws !! 5)) x)
    where operator = parseOperator (ws !! 4)
          ws = words s

parseOperator :: String -> (Int -> Int -> Int)
parseOperator "+" = (+)
parseOperator "*" = (*)

parseOperand :: Int -> String -> Int
parseOperand x "old" = x
parseOperand x int = read int

parseTest :: String -> (Int -> Bool)
parseTest s = (\x -> x `mod` y == 0)
    where y = read $ last $ words s


monkeyBusiness :: [Monkey] -> Int
monkeyBusiness monkeys = product $ take 2 $ reverse $ sort $ map getItemsInspected monkeys

runRounds :: Int -> [Monkey] -> [Monkey]
runRounds n monkeys = (iterate runRound monkeys) !! n

runRound :: [Monkey] -> [Monkey]
runRound monkeys = iterateTurns 0 monkeys

iterateTurns :: Int -> [Monkey] -> [Monkey]
iterateTurns _ [] = []
iterateTurns i monkeys
    | i < length monkeys = iterateTurns (i + 1) $ takeTurn (monkeys !! i) i monkeys
    | otherwise          = monkeys

takeTurn :: Monkey -> Int -> [Monkey] -> [Monkey]
takeTurn monkey@([], _, _, _, _) i monkeys = updateList i monkey monkeys
takeTurn (input:items, operation, test, receivers, itemsInspected) i monkeys = takeTurn updateCurrent i (updateList receiver (updateMonkey (monkeys !! receiver) output) monkeys)
    where (output, receiver) = calculateOutput input operation test receivers
          updateCurrent = (items, operation, test, receivers, itemsInspected + 1)

calculateOutput :: Int -> (Int -> Int) -> (Int -> Bool) -> (Int, Int) -> (Int, Int)
calculateOutput input operation test receivers = (output, if test output then fst receivers else snd receivers)
    where output = (operation input) `div` 3

updateMonkey :: Monkey -> Int -> Monkey
updateMonkey (items, a, b, c, d) item = (items ++ [item], a, b, c, d)

updateList :: Int -> a -> [a] -> [a]
updateList _ _ [] = []
updateList 0 new (x:xs) = (new:xs)
updateList i new (x:xs) = x : updateList (i - 1) new xs


splitOn :: String -> [String] -> [[String]]
splitOn x ys = splitOnAcc x (reverse ys) [[]] 

splitOnAcc :: String -> [String] -> [[String]] -> [[String]]
splitOnAcc _ [] acc = acc
splitOnAcc x (y:ys) (z:zs)
    | x == y    = splitOnAcc x ys ([]:(z:zs))
    | otherwise = splitOnAcc x ys ((y:z):zs)

