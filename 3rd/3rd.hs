import System.IO
import Data.List
import Data.Maybe

main = do
    contents <- readFile "input.txt"
    let rucksacks = lines contents
    print $ part1 rucksacks
    print $ part2 rucksacks
    

part1 :: [String] -> Int
part1 rucksacks = sum $ map (getScore . identifyMatching . splitString) rucksacks

part2 :: [String] -> Int
part2 rucksacks = sum $ map (getScore . identifyMatching3) $ getThrees rucksacks


splitString :: String -> (String, String)
splitString s = splitAt halfway s
    where halfway = (length s) `div` 2

identifyMatching :: (String, String) -> Char
identifyMatching (x, y) = foldl1 (\acc c -> if elem c y then c else acc) x

getScore :: Char -> Int
getScore c = (fromMaybe 0 (elemIndex c priorityList)) + 1
    where priorityList = ['a'..'z'] ++ ['A'..'Z']

getThrees :: [String] -> [(String, String, String)]
getThrees [] = []
getThrees (a:b:c:xs) = [(a, b, c)] ++ (getThrees xs)

identifyMatching3 :: (String, String, String) -> Char
identifyMatching3 (x, y, z) = foldl1 (\acc c -> if (elem c y) && (elem c z) then c else acc) x

