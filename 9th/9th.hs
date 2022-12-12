import System.IO

main = do
    contents <- readFile "input.txt"
    let input = lines contents
    print $ part1 input
    print $ part2 input

part1 :: [String] -> Int
part1 = positionsVisited . tailPositions 2 . headPositions . convert

part2 :: [String] -> Int
part2 = positionsVisited . tailPositions 10 . headPositions . convert


type Direction = Char
type Position = (Int, Int)

convert :: [String] -> [Direction]
convert = concat . map expand

expand :: String -> [Direction]
expand s = replicate n c
    where c = head s
          n = read $ last $ words s

headPositions :: [Direction] -> [Position]
headPositions = scanl move (0, 0)

move :: Position -> Direction -> Position
move (x, y) 'L' = (x - 1, y)
move (x, y) 'R' = (x + 1, y)
move (x, y) 'U' = (x, y + 1)
move (x, y) 'D' = (x, y - 1)

followerPositions :: [Position] -> [Position]
followerPositions = tail . scanl follow (0, 0)

follow :: Position -> Position -> Position
follow tail@(xt, yt) head@(xh, yh)
    | isTouching tail head = tail
    | abs (xt - xh) < 2    = (xh, midpoint yt yh)
    | abs (yt - yh) < 2    = (midpoint xt xh, yh)
    | otherwise            = (midpoint xt xh, midpoint yt yh)
    where midpoint a b = (a + b) `div` 2

isTouching :: Position -> Position -> Bool
isTouching (x1, y1) (x2, y2) = abs (x1 - x2) < 2 && abs (y1 - y2) < 2

positionsVisited :: [Position] -> Int
positionsVisited = length . distinct

distinct :: (Eq a) => [a] -> [a]
distinct = foldr (\x acc -> if any (== x) acc then acc else x : acc) []

tailPositions :: Int -> [Position] -> [Position]
tailPositions ropeLength headPositions = (iterate followerPositions headPositions) !! (ropeLength - 1)

