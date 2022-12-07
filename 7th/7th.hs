import System.IO
import Data.List

main = do
    contents <- readFile "input.txt"
    let output = lines contents
    print $ directories output


directories :: [String] -> [String]
directories output = "/" : (unique $ map (drop 4) $ filter ("dir" `isPrefixOf`) output)

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs)
    | any (== x) xs = unique xs
    | otherwise = x : unique xs

