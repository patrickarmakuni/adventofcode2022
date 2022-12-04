import System.IO
import Data.List
import Data.Maybe

main = do
    contents <- readFile "input.txt"
    let pairs = lines contents
    print $ part1 pairs
    

part1 :: [String] -> [Pair]
part1 pairs = map (parsePair) pairs

type Range = (Int, Int)
type Pair = (Range, Range)

parsePair :: String -> Pair
parsePair pairs = (parseRange(range1), parseRange(range2))
    where (range1, range2) = splitOn ',' pairs

parseRange :: String -> Range
parseRange range = (read fst, read snd)
    where (fst, snd) = splitOn '-' range

splitOn :: Eq a => a -> [a] -> ([a], [a])
splitOn elem list = (take index list, drop (index + 1) list)
    where index = fromMaybe 0 (elemIndex elem list)

