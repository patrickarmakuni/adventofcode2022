import System.IO

main = do
    contents <- readFile "input.txt"
    let input = lines contents
    print "blah"

type Position = (Int, Int)

fallsEndlessly :: [Position] -> Int -> Position -> Bool
fallsEndlessly blocked floor start = snd (trajectory !! (floor + 1)) > floor
    where trajectory = iterate (fallStep blocked) start

dropGrain :: Position -> Int -> [Position] -> [Position]
dropGrain start floor blocked = (fall blocked start floor) : blocked

fall :: [Position] -> Position -> Int -> Position
fall blocked start floor = trajectory !! floor
    where trajectory = iterate (fallStep blocked) start

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

