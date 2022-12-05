import System.IO
import Data.List
import Data.Maybe
import Data.Char

main = do
    contents <- readFile "input.txt"
    let (stacks, moves) = parseInput (lines contents)
    print stacks
    print moves


type Stack = [Char]
type Row = [Char]
type Move = (Int, Int, Int)

parseInput :: [String] -> ([Stack], [Move])
parseInput input = (parseStacks stacks, parseMoves moves)
    where (stacks, moves) = splitOn "" input

parseMoves :: [String] -> [(Int, Int, Int)]
parseMoves moves = map (parseMove) moves

parseMove :: String -> (Int, Int, Int)
parseMove move = (getInt ws 1, getInt ws 3, getInt ws 5)
    where ws = words move
          getInt ws idx = read (ws !! idx)

parseStacks :: [String] -> [Stack]
parseStacks stacks = map (trim) (transpose rows)
    where rows = parseRows actualStacks
          actualStacks = init stacks
          trim = dropWhileEnd isSpace . dropWhile isSpace

parseRows :: [String] -> [Row]
parseRows [] = []
parseRows (top:rows) = (parseRow top) : parseRows rows

parseRow :: String -> Row
parseRow str = map (\idx -> str !! idx) indices
    where indices = [1,5..(length str)]

splitOn :: Eq a => a -> [a] -> ([a], [a])
splitOn elem list = (take index list, drop (index + 1) list)
    where index = fromMaybe 0 (elemIndex elem list)


