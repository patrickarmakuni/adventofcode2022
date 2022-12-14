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


dropGrain :: Position -> Int -> [Position] -> [Position]
dropGrain start floor blocked = maybeToList (fall blocked start floor) ++ blocked

fall :: [Position] -> Position -> Int -> Maybe Position
fall blocked current@(x, y) floor
    | y >= floor      = Nothing
    | next == current = Just current
    | otherwise       = fall blocked next floor
    where next = fallStep blocked current

fallStep :: [Position] -> Position -> Position
fallStep blocked current@(x, y)
    | isFree below      = below
    | isFree belowLeft  = belowLeft
    | isFree belowRight = belowRight
    | otherwise         = current
    where below      = (x, y + 1)
          belowLeft  = (x - 1, y + 1)
          belowRight = (x + 1, y + 1)
          isFree p = all (/= p) blocked


splitOn :: String -> [String] -> [[String]]
splitOn x ys = splitOnAcc x (reverse ys) [[]]

splitOnAcc :: String -> [String] -> [[String]] -> [[String]]
splitOnAcc _ [] acc = acc
splitOnAcc x (y:ys) (z:zs)
    | x == y    = splitOnAcc x ys ([]:(z:zs))
    | otherwise = splitOnAcc x ys ((y:z):zs)

