import System.IO

main = do
    contents <- readFile "testInput.txt"
    let input = lines contents
    print $ follow $ convert input


type Direction = Char
type Position = (Int, Int)

convert :: [String] -> [Direction]
convert = concat . map expand

expand :: String -> [Direction]
expand s = replicate n c
    where c = head s
          n = read $ last $ words s

follow :: [Direction] -> [Position]
follow = scanl move (0, 0)

move :: Position -> Direction -> Position
move (x, y) 'L' = (x - 1, y)
move (x, y) 'R' = (x + 1, y)
move (x, y) 'U' = (x, y + 1)
move (x, y) 'D' = (x, y - 1)

