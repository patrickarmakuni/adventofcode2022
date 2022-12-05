import System.IO
import Data.List
import Data.Maybe

main = do
    contents <- readFile "input.txt"
    let (stacks, moves) = parseInput (lines contents)
    print stacks


type Stack = [Char]
type Move = String

parseInput :: [String] -> ([Stack], [Move])
parseInput input = (parseStacks stacks, moves)
    where (stacks, moves) = splitOn "" input

parseStacks :: [String] -> [Stack]
parseStacks stacks = transpose rows
    where rows = parseRows actualStacks
          actualStacks = init stacks

parseRows :: [String] -> [[Char]]
parseRows [] = []
parseRows (top:rows) = (parseRow top) : parseRows rows

parseRow :: String -> [Char]
parseRow str = map (\idx -> str !! idx) indices
    where indices = [1,5..(length str)]

splitOn :: Eq a => a -> [a] -> ([a], [a])
splitOn elem list = (take index list, drop (index + 1) list)
    where index = fromMaybe 0 (elemIndex elem list)
