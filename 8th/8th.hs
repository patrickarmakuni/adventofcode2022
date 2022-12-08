import System.IO
import Data.Char
import Data.List

main = do
    contents <- readFile "input.txt"
    let grid = readDigits $ lines contents
    print $ gridSum $ gridOr [vfl grid, vfr grid, vft grid, vfb grid]


gridSum :: [[Bool]] -> Int
gridSum grid = sum $ map count grid

count :: [Bool] -> Int
count bools = length $ filter (== True) bools

gridOr :: [[[Bool]]] -> [[Bool]]
gridOr grids = foldl1 (zipWith (zipWith (||))) grids

vfl :: [[Int]] -> [[Bool]]
vfl grid = map (\row -> visibleFromLeft row (-1) []) grid

vfr :: [[Int]] -> [[Bool]]
vfr grid = map (\row -> visibleFromRight row (-1) []) grid

vft :: [[Int]] -> [[Bool]]
vft grid = transpose $ vfl $ transpose grid

vfb :: [[Int]] -> [[Bool]]
vfb grid = transpose $ vfr $ transpose grid

visibleFromLeft :: [Int] -> Int -> [Bool] -> [Bool]
visibleFromLeft [] _ acc = acc
visibleFromLeft (tree:row) highest acc
    | tree > highest = visibleFromLeft row tree (acc ++ [True])
    | otherwise      = visibleFromLeft row highest (acc ++ [False])

visibleFromRight :: [Int] -> Int -> [Bool] -> [Bool]
visibleFromRight row _ _ = reverse (visibleFromLeft (reverse row) (-1) [])

readDigits :: [String] -> [[Int]]
readDigits strings = map (map digitToInt) strings

