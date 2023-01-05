import System.Environment
import System.IO
import Data.List
import Data.Char

main = do
    args <- getArgs
    let inputFile = args !! 0
    output <- fmap lines $ readFile inputFile
    print $ part1 output
    print $ part2 output


data Contents = File Int | Directory String [Contents]

instance Show Contents where show = showAcc 0

showAcc :: Int -> Contents -> String
showAcc n (Directory name contents) = indent n $ name ++ "\n" ++ (intercalate "\n" $ map (showAcc (n+1)) contents)
showAcc n (File x) = indent n $ show x

indent :: Int -> String -> String
indent n s = (replicate (2 * n) ' ') ++ s

part1 :: [String] -> Int
part1 output = sumSub100000 $ buildDirectory output

sumSub100000 :: Contents -> Int
sumSub100000 (File _) = 0
sumSub100000 dir@(Directory _ contents) = dirValue + (sum $ map sumSub100000 contents)
    where s = size dir
          dirValue = if s <= 100000 then s else 0

part2 :: [String] -> Int
part2 output = smallestGreaterThan deficit totalSize fileSystem
    where fileSystem = buildDirectory output
          totalSize = size fileSystem
          deficit = totalSize - 40000000

smallestGreaterThan :: Int -> Int -> Contents -> Int
smallestGreaterThan n acc (File _) = acc
smallestGreaterThan n acc dir@(Directory _ contents) = minimum $ map (smallestGreaterThan n smallest) contents
    where smallest = if currentSize < acc && currentSize > n then currentSize else acc
          currentSize = size dir

size :: Contents -> Int
size (File x) = x
size (Directory _ contents) = sum $ map size contents

buildDirectory :: [String] -> Contents
buildDirectory output = Directory name $ files ++ subdirs
    where files = map (getFile) $ filter (isFile) $ getCurrentDirContents output
          subdirs = map (buildDirectory) $ getBlocks $ nextSection output
          name = last $ words $ head output


nextSection :: [String] -> [String]
nextSection input = dropWhileEnd (== "$ cd ..") $ dropWhile (not . isCommand) $ dropWhile (isCommand) input
    where isCommand = isPrefixOf "$ "

getCurrentDirContents :: [String] -> [String]
getCurrentDirContents output = takeWhile (not . isCommand) $ dropWhile (isCommand) output
    where isCommand = isPrefixOf "$ "

getBlocks :: [String] -> [[String]]
getBlocks [] = []
getBlocks input = splitAt' idxs input
    where idxs = getSplitIndices input 0 0 []

splitAt' :: (Eq a) => [Int] -> [a] -> [[a]]
splitAt' [] input = [input]
splitAt' (idx:idxs) input = [(take n input)] ++ (splitAt' (map (subtract n) idxs) (drop n input))
    where n = idx + 1

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

isFile :: String -> Bool
isFile = (isDigit . head)

isDir :: String -> Bool
isDir s = "dir " `isPrefixOf` s

isCommand :: String -> Bool
isCommand s = "$ " `isPrefixOf` s

getFile :: String -> Contents
getFile = File . read . head . words

