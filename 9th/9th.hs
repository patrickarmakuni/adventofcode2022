import System.IO

main = do
    contents <- readFile "input.txt"
    let input = lines contents
    print $ length $ distinct $ tailPositions $ headPositions $ convert input


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

tailPositions :: [Position] -> [Position]
tailPositions = tail . scanl follow (0, 0)

follow :: Position -> Position -> Position
follow tail@(xt, yt) head@(xh, yh)
    | isTouching tail head = tail
    | abs (xt - xh) < 2    = (xh, (yt + yh) `div` 2)
    | abs (yt - yh) < 2    = ((xt + xh) `div` 2, yh)

isTouching :: Position -> Position -> Bool
isTouching (x1, y1) (x2, y2) = abs (x1 - x2) < 2 && abs (y1 - y2) < 2

distinct :: (Eq a) => [a] -> [a]
distinct [] = []
distinct (x:xs)
    | any (== x) xs = distinct xs
    | otherwise     = x : (distinct xs)

