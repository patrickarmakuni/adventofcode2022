import System.IO
import Data.List

main = do
    contents <- readFile "testInput.txt"
    let output = lines contents
    print $ part1 output


part1 :: [String] -> Int
part1 output = sum $ filter (<= 100000) $ map (calculateSize output) $ directories output

calculateSize :: [String] -> String -> Int
calculateSize output directory = foldl (\acc line -> if isFile line then acc + (getFileSize line) else acc + (calculateSize output (drop 4 line))) 0 contents
    where contents = getDirContents output directory
          isFile = not . isDirectory
          isDirectory = isPrefixOf "dir"

getDirContents :: [String] -> String -> [String]
getDirContents output dir = takeWhile (not . isCommand) $ dropWhile (isCommand) $ dropWhile (/= "$ cd " ++ dir) output
    where isCommand = isPrefixOf "$ "

getFileSize :: String -> Int
getFileSize line = read $ head $ words line

directories :: [String] -> [String]
directories output = "/" : (unique $ map (drop 4) $ filter ("dir" `isPrefixOf`) output)

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs)
    | any (== x) xs = unique xs
    | otherwise = x : unique xs

