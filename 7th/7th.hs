import System.IO
import Data.List
import Data.Char

main = do
    contents <- readFile "input.txt"
    let output = lines contents
    print $ part1 output


part1 :: [String] -> Int
part1 output = getSize output

getSize :: [String] -> Int
getSize [] = 0
getSize input = sumFilesInDir + sumSubdirs
    where sumFilesInDir = sum $ map (getFileSize) $ filter (isFile) $ getCurrentDirContents input
          sumSubdirs = sum $ subdirSizes
          subdirSizes = map (getSize) $ getBlocks nextSection
          nextSection = dropWhile (not . isCommand) $ dropWhile (isCommand) input
          isCommand = isPrefixOf "$ "
          isFile = (isDigit . head)

getCurrentDirContents :: [String] -> [String]
getCurrentDirContents output = takeWhile (not . isCommand) $ dropWhile (isCommand) output
    where isCommand = isPrefixOf "$ "

getBlocks :: [String] -> [[String]]
getBlocks input = splitAt' idxs input
    where idxs = getSplitIndices input 0 0 []

splitAt' :: (Eq a) => [Int] -> [a] -> [[a]]
splitAt' [] input = [input]
splitAt' (idx:idxs) input = [(take idx input)] ++ (splitAt' (map (subtract idx) idxs) (drop (idx + 1) input))

getSplitIndices :: [String] -> Int -> Int -> [Int] -> [Int]
getSplitIndices [] _ _ acc = acc
getSplitIndices (line:rest) level idx acc
    | isCdUp line = if level == 1 then getSplitIndices rest 0 (idx + 1) (idx : acc) else getSplitIndices rest (level - 1) (idx + 1) acc
    | isCdDown line = getSplitIndices rest (level + 1) (idx + 1) acc
    | otherwise = getSplitIndices rest level (idx + 1) acc
    where isCdUp l = l == "$ cd .."
          isCdDown l = "$ cd" `isPrefixOf` l

getFileSize :: String -> Int
getFileSize = read . head . words

