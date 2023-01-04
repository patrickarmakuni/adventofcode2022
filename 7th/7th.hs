import System.Environment
import System.IO
import Data.List
import Data.Char

main = do
    args <- getArgs
    let inputFile = args !! 0
    output <- fmap lines $ readFile inputFile
    print $ part1 output


part1 :: [String] -> [Int]
part1 output = getSize output

getSize :: [String] -> [Int]
getSize [] = []
getSize input = (sumFilesInDir + sumSubdirs) : subdirSizes
    where sumFilesInDir = sum $ map (getFileSize) $ filter (isFile) $ getCurrentDirContents input
          sumSubdirs = sum $ map (headOr 0) subdirs
          subdirSizes = concat subdirs
          subdirs = map (getSize) $ getBlocks $ nextSection input
          isFile = (isDigit . head)

nextSection :: [String] -> [String]
nextSection input = dropWhileEnd (== "$ cd ..") $ dropWhile (not . isCommand) $ dropWhile (isCommand) input
    where isCommand = isPrefixOf "$ "

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
    | isCdUp line = if level == 1 then getSplitIndices rest 0 (idx + 1) (acc ++ [idx]) else getSplitIndices rest (level - 1) (idx + 1) acc
    | isCdDown line = getSplitIndices rest (level + 1) (idx + 1) acc
    | otherwise = getSplitIndices rest level (idx + 1) acc
    where isCdUp l = l == "$ cd .."
          isCdDown l = "$ cd" `isPrefixOf` l

getFileSize :: String -> Int
getFileSize = read . head . words

headOr :: a -> [a] -> a
headOr x [] = x
headOr _ (x:_) = x

