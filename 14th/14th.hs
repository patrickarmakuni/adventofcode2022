import System.IO
import System.Environment
import Data.List
import Data.Maybe


main = do
    args <- getArgs
    let fileName = args !! 0
    input <- fmap lines $ readFile fileName
    print $ part1 input
    print $ part2 input


type Position = (Int, Int)
type Row = [Int]


part1 :: [String] -> Int
part1 input = (subtract 1) $ countWhile floorIsNotReached structures
    where structures = addFloor $ convertToRows $ getStructures input
          floorIsNotReached rows = null (rows !! ((length rows) - 2))

part2 :: [String] -> Int
part2 input = countWhile topRowIsEmpty structures
    where structures = addFloor $ convertToRows $ getStructures input
          topRowIsEmpty rows = null (rows !! 0)

countWhile :: ([Row] -> Bool) -> [Row] -> Int
countWhile condition structures = length $ takeWhile condition $ iterate dropGrain structures


dropGrain :: [Row] -> [Row]
dropGrain blocked = addGrain (fall blocked start) blocked
    where start = (500, 0)

addGrain :: Position -> [Row] -> [Row]
addGrain (x, y) rows = (take y rows) ++ [(x : (rows !! y))] ++ (drop (y+1) rows)

fall :: [Row] -> Position -> Position
fall blocked current@(x, y)
    | next == current = current
    | otherwise = fall blocked next
    where next = fallStep blocked current

fallStep :: [Row] -> Position -> Position
fallStep blocked current@(x, y)
    | isFree x          = (x, y + 1)
    | isFree (x - 1)    = (x - 1, y + 1)
    | isFree (x + 1)    = (x + 1, y + 1)
    | otherwise         = current
    where isFree x = all (/= x) (blocked !! (y + 1))


convertToRows :: [Position] -> [Row]
convertToRows positions = reverse $ foldl (\acc y -> (getRow y) : acc) [] [0..lowest]
    where getRow row = map fst $ filter (\(_, y) -> y == row) positions
          lowest = maximum $ map snd positions

addFloor :: [Row] -> [Row]
addFloor structures = structures ++ [[], [(500 - height)..(500 + height)]]
    where height = length structures + 2

getStructures :: [String] -> [Position]
getStructures input = concat $ map (buildStructure . points) input

buildStructure :: [Position] -> [Position]
buildStructure (p:[]) = []
buildStructure (p1:p2:xs) = (drawLine p1 p2) ++ buildStructure (p2 : xs)

points :: String -> [Position]
points = map parsePosition . filter (/= "->") . words

drawLine :: Position -> Position -> [Position]
drawLine (x1, y1) (x2, y2)
    | x1 == x2 = map (\y -> (x1, y)) $ range y1 y2
    | y1 == y2 = map (\x -> (x, y1)) $ range x1 x2
    where range a b = if a < b then [a..b] else [b..a]

parsePosition :: String -> Position
parsePosition s = (read x, read $ tail y)
    where (x, y) = splitAt 3 s


splitOn :: String -> [String] -> [[String]]
splitOn x ys = splitOnAcc x (reverse ys) [[]]

splitOnAcc :: String -> [String] -> [[String]] -> [[String]]
splitOnAcc _ [] acc = acc
splitOnAcc x (y:ys) (z:zs)
    | x == y    = splitOnAcc x ys ([]:(z:zs))
    | otherwise = splitOnAcc x ys ((y:z):zs)

