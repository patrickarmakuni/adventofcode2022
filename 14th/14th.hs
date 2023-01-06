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
part1 input = findSteadyState 0 $ iterate (dropGrain1 (500, 0) floor) rows
    where blocked = getStructures input
          rows = convertToRows (floor + 1) blocked
          floor = maximum $ map snd blocked

findSteadyState :: Int -> [[Row]] -> Int
findSteadyState idx (t1:t2:ts)
    | t1 == t2 = idx
    | otherwise = findSteadyState (idx + 1) (t2:ts)

dropGrain1 :: Position -> Int -> [Row] -> [Row]
dropGrain1 start floor blocked = addGrain1 (fall1 blocked floor start) blocked

addGrain1 :: Maybe Position -> [Row] -> [Row]
addGrain1 Nothing rows = rows
addGrain1 (Just (x, y)) rows = (take y rows) ++ [(x : (rows !! y))] ++ (drop (y+1) rows)

fall1 :: [Row] -> Int -> Position -> Maybe Position
fall1 blocked floor current@(x, y)
    | y >= floor = Nothing
    | next == current = Just current
    | otherwise = fall1 blocked floor next
    where next = fallStep blocked current


part2 :: [String] -> Int
part2 input = length $ takeWhile (\rows -> null (rows !! 0)) $ iterate (dropGrain2 (500, 0) floor) rows
    where blocked = getStructures input
          rows = convertToRows floor blocked
          floor = (maximum $ map snd blocked) + 2

dropGrain2 :: Position -> Int -> [Row] -> [Row]
dropGrain2 start floor blocked = addGrain2 (fall2 blocked floor start) blocked

addGrain2 :: Position -> [Row] -> [Row]
addGrain2 (x, y) rows = (take y rows) ++ [(x : (rows !! y))] ++ (drop (y+1) rows)

fall2 :: [Row] -> Int -> Position -> Position
fall2 blocked floor current@(x, y)
    | y + 1 == floor = current
    | next == current = current
    | otherwise = fall2 blocked floor next
    where next = fallStep blocked current


fallStep :: [Row] -> Position -> Position
fallStep blocked current@(x, y)
    | isFree x          = (x, y + 1)
    | isFree (x - 1)    = (x - 1, y + 1)
    | isFree (x + 1)    = (x + 1, y + 1)
    | otherwise         = current
    where isFree x = all (/= x) (blocked !! (y + 1))


convertToRows :: Int -> [Position] -> [Row]
convertToRows floor positions = reverse $ foldl (\acc y -> (getRow y) : acc) [] [0..(floor-1)]
    where getRow row = map fst $ filter (\(_, y) -> y == row) positions

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

