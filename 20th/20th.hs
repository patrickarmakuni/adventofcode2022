import System.IO
import System.Environment
import Data.List
import Data.Maybe


main = do
    args <- getArgs
    let inputFile = args !! 0
    input <- fmap lines $ readFile inputFile
    let numbers = map read input :: [Int]
    print $ part1 numbers
    print $ part2 numbers


type Index = Int


part1 :: [Int] -> Int
part1 numbers = sum $ coordinates $ valuesAt (mix numbers indices) numbers
    where indices = [0..(length numbers - 1)]

part2 :: [Int] -> Int
part2 numbers = sum $ coordinates $ valuesAt mix10 decrypted
    where decrypted = map (* 811589153) numbers
          indices = [0..(length numbers - 1)]
          mix10 = (iterate (mix decrypted) indices) !! 10


getOrder :: Index -> [Index] -> [Index]
getOrder x xs
    | x >= length xs = []
    | otherwise = idx : getOrder (x + 1) xs
    where idx = fromJust $ elemIndex x xs

valuesAt :: [Index] -> [a] -> [a]
valuesAt indices xs = map (\idx -> xs !! idx) indices

mix :: [Int] -> [Index] -> [Index]
mix numbers indices = mixAcc order indices numbers
    where order = getOrder 0 indices

mixAcc :: [Index] -> [Index] -> [Int] -> [Int]
mixAcc [] state _ = state
mixAcc (idx:indices) state (value:numbers) = mixAcc remaining newState numbers
    where newState = relocate idx destination state
          remaining = map (updateIndex idx destination) indices
          destination = (idx + value) `mod` (length state - 1)

updateIndex :: Index -> Index -> Index -> Index
updateIndex from to idx
    | idx < min from to = idx
    | idx > max from to = idx
    | from < to         = idx - 1
    | from > to         = idx + 1
    | otherwise         = idx

relocate :: Index -> Index -> [a] -> [a]
relocate from to xs = insertAt to value $ deleteAt from xs
    where value = xs !! from

deleteAt :: Index -> [a] -> [a]
deleteAt idx xs = (take idx xs) ++ (drop (idx + 1) xs)

insertAt :: Index -> a -> [a] -> [a]
insertAt idx value xs = (take idx xs) ++ [value] ++ (drop idx xs)

coordinates :: [Int] -> [Int]
coordinates numbers = [getValue 1000, getValue 2000, getValue 3000]
    where getValue n = numbers !! ((zero + n) `mod` (length numbers))
          zero = fromJust $ elemIndex 0 numbers

