import System.IO
import System.Environment
import Data.Char
import Data.List

main = do
    contents <- readFile "input.txt"
    let input = lines contents
    print $ part1 input
    print $ part2 input


part1 :: [String] -> Int
part1 input = monkeyBusiness $ runRounds 20 monkeys
    where monkeys = parseMonkeys (\x -> x `div` 3) input

part2 :: [String] -> Int
part2 input = monkeyBusiness $ runRounds 10000 monkeys
    where monkeys = parseMonkeys (\x -> x `mod` modulus) input
          modulus = product $ testValues input

testValues :: [String] -> [Int]
testValues = map parseTestValue . filter isTest
    where isTest s = s /= "" && (head . words) s == "Test:"


type Monkey = ([Item], Operation, TestValue, (Int, Int), Int)
type Item = Int
type Operation = Int -> Int
type TestValue = Int

getItemsInspected :: Monkey -> Int
getItemsInspected (_, _, _, _, itemsInspected) = itemsInspected


parseMonkeys :: (Int -> Int) -> [String] -> [Monkey]
parseMonkeys reduce = map (parseMonkey reduce) . splitOn ""

parseMonkey :: (Int -> Int) -> [String] -> Monkey
parseMonkey reduce input = (items, operation, testValue, receivers, 0)
    where items = map parseInt $ drop 2 $ words $ input !! 1
          operation = reduce . (parseOperation $ input !! 2)
          testValue = parseTestValue $ input !! 3
          receivers = (digitToInt $ last $ input !! 4, digitToInt $ last $ input !! 5)

parseInt :: String -> Int
parseInt = read . takeWhile isDigit

parseOperation :: String -> Operation
parseOperation s = (\x -> operator (parseOperand x (ws !! 5)) x)
    where operator = parseOperator (ws !! 4)
          ws = words s

parseOperator :: String -> (Int -> Int -> Int)
parseOperator "+" = (+)
parseOperator "*" = (*)

parseOperand :: Int -> String -> Int
parseOperand x "old" = x
parseOperand x int = read int

parseTestValue :: String -> TestValue
parseTestValue = read . last . words


monkeyBusiness :: [Monkey] -> Int
monkeyBusiness = product . take 2 . reverse . sort . map getItemsInspected

runRounds :: Int -> [Monkey] -> [Monkey]
runRounds n monkeys = (iterate runRound monkeys) !! n

runRound :: [Monkey] -> [Monkey]
runRound = iterateTurns 0

iterateTurns :: Int -> [Monkey] -> [Monkey]
iterateTurns _ [] = []
iterateTurns i monkeys
    | i < length monkeys = iterateTurns (i + 1) $ takeTurn (monkeys !! i) i monkeys
    | otherwise          = monkeys

takeTurn :: Monkey -> Int -> [Monkey] -> [Monkey]
takeTurn monkey@([], _, _, _, _) i monkeys = updateList i monkey monkeys
takeTurn (input:items, operation, testValue, receivers, itemsInspected) i monkeys = takeTurn updateCurrent i (updateList receiver (updateMonkey (monkeys !! receiver) output) monkeys)
    where (output, receiver) = calculateOutput input operation testValue receivers
          updateCurrent = (items, operation, testValue, receivers, itemsInspected + 1)

calculateOutput :: Item -> Operation -> TestValue -> (Int, Int) -> (Int, Int)
calculateOutput input operation testValue receivers = (output, if (output `mod` testValue) == 0 then fst receivers else snd receivers)
    where output = operation input

updateMonkey :: Monkey -> Item -> Monkey
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

