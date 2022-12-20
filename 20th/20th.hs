import System.IO
import System.Environment
import Data.List
import Data.Maybe


main = do
    args <- getArgs
    let inputFile = args !! 0
    input <- fmap lines $ readFile inputFile
    let numbers = map read input :: [Int]
    print $ sum $ coordinates $ mix numbers


type Index = Int


mix :: [Int] -> [Int]
mix numbers = mixWithIndices [0..(length numbers - 1)] numbers

mixWithIndices :: [Index] -> [Int] -> [Int]
mixWithIndices [] numbers = numbers
mixWithIndices (idx:indices) numbers = mixWithIndices newIndices newNumbers
    where newNumbers = relocate idx destination numbers
          newIndices = map (updateIndex idx destination) indices
          destination = (idx + value) `mod` (length numbers - 1)
          value = numbers !! idx

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

