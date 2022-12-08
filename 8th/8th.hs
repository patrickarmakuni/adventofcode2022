import System.IO
import Data.Char
import Data.List

main = do
    contents <- readFile "input.txt"
    let grid = readDigits $ lines contents
    print $ part1 grid
    print $ part2 grid


type Grid = [Row]
type Row = [Int]


part1 :: Grid -> Int
part1 grid =  gridCount (> 0) $ gridSum [vfl grid, vfr grid, vft grid, vfb grid]

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


part2 :: Grid -> Int
part2 grid = maximum $ map maximum $ gridProduct [vtl grid, vtr grid, vtt grid, vtb grid]

vtr :: Grid -> Grid
vtr = map vtrRow

vtl :: Grid -> Grid
vtl = map vtlRow

vtt :: Grid -> Grid
vtt = transpose . vtl . transpose

vtb :: Grid -> Grid
vtb = transpose . vtr . transpose

vtlRow :: Row -> Row
vtlRow = reverse . vtrRow . reverse

vtrRow :: Row -> Row
vtrRow row = map visibleTreesRight subrows
    where subrows = init $ scanr (:) [] row

visibleTreesRight :: Row -> Int
visibleTreesRight [] = 0
visibleTreesRight (tree:[]) = 0
visibleTreesRight (tree:row) = length (takeUntil (>= tree) row)


gridCount :: (Int -> Bool) -> Grid -> Int
gridCount p = sum . map count
    where count = length . filter p

gridSum :: [Grid] -> Grid
gridSum = foldl1 (zipWith $ zipWith (+))

gridProduct :: [Grid] -> Grid
gridProduct = foldl1 (zipWith $ zipWith (*))

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs)
    | p x = [x]
    | otherwise = x : takeUntil p xs

readDigits :: [String] -> Grid
readDigits = map $ map digitToInt

