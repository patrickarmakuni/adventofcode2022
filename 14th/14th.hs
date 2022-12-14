import System.IO
import Data.List
import Data.Maybe


main = do
    input <- fmap lines $ readFile "input.txt"
    print $ part1 input


type Position = (Int, Int)

part1 :: [String] -> Int
part1 input = findSteadyState 0 $ iterate (dropGrain (500, 0) floor) blocked
    where blocked = getStructures input
          floor = maximum $ map (snd) blocked

findSteadyState :: Int -> [[Position]] -> Int
findSteadyState idx (x:y:xs)
    | x == y = idx
    | otherwise = findSteadyState (idx + 1) (y:xs)


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


fallsEndlessly :: [Position] -> Int -> Position -> Bool
fallsEndlessly blocked floor start = snd (trajectory !! (floor + 1)) > floor
    where trajectory = iterate (fallStep blocked) start

dropGrain :: Position -> Int -> [Position] -> [Position]
dropGrain start floor blocked = maybeToList (fall blocked start floor) ++ blocked

fall :: [Position] -> Position -> Int -> Maybe Position
fall blocked current@(x, y) floor
    | y >= floor      = Nothing
    | next == current = Just current
    | otherwise       = fall blocked next floor
    where next = fallStep blocked current

restingPosition :: [Position] -> Position
restingPosition (x:y:xs)
    | x == y    = x
    | otherwise = restingPosition xs

fallStep :: [Position] -> Position -> Position
fallStep blocked current@(x, y) = getCandidate blocked current candidates
    where candidates = [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]

getCandidate :: [Position] -> Position -> [Position] -> Position
getCandidate _ current [] = current
getCandidate blocked current (x:xs)
    | any (== x) blocked = getCandidate blocked current xs
    | otherwise = x


splitOn :: String -> [String] -> [[String]]
splitOn x ys = splitOnAcc x (reverse ys) [[]]

splitOnAcc :: String -> [String] -> [[String]] -> [[String]]
splitOnAcc _ [] acc = acc
splitOnAcc x (y:ys) (z:zs)
    | x == y    = splitOnAcc x ys ([]:(z:zs))
    | otherwise = splitOnAcc x ys ((y:z):zs)

