import System.IO

main = do
    contents <- readFile "input.txt"
    let input = lines contents
    print "blah"

type Position = (Int, Int)

fallStep :: [Position] -> Position -> Position
fallStep blocked current@(x, y) = getCandidate blocked current candidates
    where candidates = [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]

getCandidate :: [Position] -> Position -> [Position] -> Position
getCandidate _ current [] = current
getCandidate blocked current (x:xs)
    | any (== x) blocked = getCandidate blocked current xs
    | otherwise = x

