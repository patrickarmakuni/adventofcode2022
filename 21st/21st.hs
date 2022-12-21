import System.IO
import System.Environment
import Data.List
import Data.Maybe


main = do
    args <- getArgs
    let inputFile = args !! 0
    input <- fmap lines $ readFile inputFile
    let monkeys = map parseMonkey input
    print $ part1 monkeys
    print $ part2 monkeys


type Monkey = [String]


part1 :: [Monkey] -> Int
part1 monkeys = calculateNumber monkeys $ findMonkey "root" monkeys

part2 :: [Monkey] -> Int
part2 monkeys = getNumber 0 monkeys $ findMonkey "root" monkeys


getNumber :: Int -> [Monkey] -> Monkey -> Int
getNumber acc _ ("humn":_) = acc
getNumber _ monkeys ("root":m1:_:m2:[])
    | isDetermined left monkeys = getNumber (calculateNumber monkeys left) monkeys $ right
    | otherwise                 = getNumber (calculateNumber monkeys right) monkeys $ left
    where left = findMonkey m1 monkeys
          right = findMonkey m2 monkeys
getNumber acc monkeys (name:m1:op:m2:[])
    | isDetermined left monkeys = getNumber (inverse op acc (calculateNumber monkeys left)) monkeys $ right
    | otherwise                 = getNumber (inverse op acc (calculateNumber monkeys right)) monkeys $ left
    where left = findMonkey m1 monkeys
          right = findMonkey m2 monkeys

isDetermined :: Monkey -> [Monkey] -> Bool
isDetermined ("humn":_) _            = False
isDetermined (name:numString:[]) _   = True
isDetermined (name:m1:_:m2:[]) monkeys = isDetermined left monkeys && isDetermined right monkeys
    where left = findMonkey m1 monkeys
          right = findMonkey m2 monkeys

findMonkey :: String -> [Monkey] -> Monkey
findMonkey name = fromJust . find (\m -> head m == name)

calculateNumber :: [Monkey] -> Monkey -> Int
calculateNumber monkeys (name:numString:[]) = read numString
calculateNumber monkeys (name:m1:op:m2:[]) = evaluate op left right
    where left = calculateNumber monkeys $ findMonkey m1 monkeys
          right = calculateNumber monkeys $ findMonkey m2 monkeys

inverse :: String -> Int -> Int -> Int
inverse "+" l r = l - r
inverse "-" l r = l + r
inverse "*" l r = l `div` r
inverse "/" l r = l * r

evaluate :: String -> Int -> Int -> Int
evaluate "+" l r = l + r
evaluate "-" l r = l - r
evaluate "*" l r = l * r
evaluate "/" l r = l `div` r

parseMonkey :: String -> Monkey
parseMonkey = removeColon . words
    where removeColon (x:xs) = (init x) : xs
