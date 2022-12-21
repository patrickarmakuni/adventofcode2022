import System.IO
import System.Environment
import Data.List
import Data.Maybe


main = do
    args <- getArgs
    let inputFile = args !! 0
    input <- fmap lines $ readFile inputFile
    let monkeys = map parseMonkey input
    print $ calculateNumber monkeys $ findMonkey "root" monkeys


type Monkey = [String]


findMonkey :: String -> [Monkey] -> Monkey
findMonkey name = fromJust . find (\m -> head m == name)

calculateNumber :: [Monkey] -> Monkey -> Int
calculateNumber monkeys (name:numString:[]) = read numString
calculateNumber monkeys (name:m1:op:m2:[]) = evaluate op left right
    where left = calculateNumber monkeys $ findMonkey m1 monkeys
          right = calculateNumber monkeys $ findMonkey m2 monkeys

evaluate :: String -> Int -> Int -> Int
evaluate "+" l r = l + r
evaluate "-" l r = l - r
evaluate "*" l r = l * r
evaluate "/" l r = l `div` r

parseMonkey :: String -> Monkey
parseMonkey = removeColon . words
    where removeColon (x:xs) = (init x) : xs
