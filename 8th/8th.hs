import System.IO
import Data.Char
import Data.List

main = do
    contents <- readFile "input.txt"
    let grid = readDigits $ lines contents
    print $ gridCount $ sumGrids [vfl grid, vfr grid, vft grid, vfb grid]


type Grid = [Row]
type Row = [Int]

gridCount :: Grid -> Int
gridCount = sum . map count
    where count = length . filter (> 0)

sumGrids :: [Grid] -> Grid
sumGrids = foldl1 (zipWith $ zipWith (+))

vfl :: Grid -> Grid
vfl = map visibleFromLeft

vfr :: Grid -> Grid
vfr = map visibleFromRight

vft :: Grid -> Grid
vft = transpose . vfl . transpose

vfb :: Grid -> Grid
vfb = transpose . vfr . transpose

visibleFromRight :: Row -> Row
visibleFromRight = reverse . visibleFromLeft . reverse

visibleFromLeft :: Row -> Row
visibleFromLeft = visibleFromLeftAcc (-1) []

visibleFromLeftAcc :: Int -> Row -> Row -> Row
visibleFromLeftAcc _ acc [] = acc
visibleFromLeftAcc highest acc (tree:row)
    | tree > highest = visibleFromLeftAcc tree (acc ++ [1]) row
    | otherwise      = visibleFromLeftAcc highest (acc ++ [0]) row

readDigits :: [String] -> Grid
readDigits = map $ map digitToInt

