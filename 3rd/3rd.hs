import System.IO
import Data.List
import Data.Maybe

main = do
    contents <- readFile "input.txt"
    let rucksacks = lines contents
        scores = map (\x -> calculateScore x) rucksacks
    print $ sum scores
    

calculateScore :: String -> Int
calculateScore s = getScore $ identifyMatching $ splitString s

splitString :: String -> (String, String)
splitString s = splitAt halfway s
    where halfway = (length s) `div` 2

identifyMatching :: (String, String) -> Char
identifyMatching (x, y) = foldl1 (\acc c -> if elem c y then c else acc) x

getScore :: Char -> Int
getScore c = (fromMaybe 0 (elemIndex c priorityList)) + 1
    where priorityList = ['a'..'z'] ++ ['A'..'Z']

