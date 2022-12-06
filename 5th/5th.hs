import System.IO
import Data.List
import Data.Maybe
import Data.Char

main = do
    contents <- readFile "input.txt"
    let (stacks, moves) = parseInput (lines contents)
    print $ part1 stacks moves
    print $ part2 stacks moves


type Stack = [Char]
type Row = [Char]
type Move = (Int, Int, Int)


part1 :: [Stack] -> [Move] -> String
part1 stacks moves = map (head) (processMoves stacks moves)
    where processMoves stacks moves = foldl (\acc m -> processMove acc m) stacks moves

part2 :: [Stack] -> [Move] -> String
part2 stacks moves = map (head) (processMoves stacks moves)
    where processMoves stacks moves = foldl (\acc m -> processMove' acc m) stacks moves


processMove :: [Stack] -> Move -> [Stack]
processMove stacks (0, _, _) = stacks
processMove stacks (number, origin, destination) = processMove (move1 stacks origin destination) (number - 1, origin, destination)

processMove' :: [Stack] -> Move -> [Stack]
processMove' stacks (number, origin, destination) = (addToDestination . removeFromOrigin) stacks
    where crates = take number (stacks !! origin)
          removeFromOrigin s = (take origin s) ++ [(drop number (s !! origin))] ++ (drop (origin + 1) s)
          addToDestination s = (take destination s) ++ [(crates ++ (s !! destination))] ++ (drop (destination + 1) s)

move1 :: [Stack] -> Int -> Int -> [Stack]
move1 stacks origin destination = (addToDestination . removeFromOrigin) stacks
    where crate = head (stacks !! origin)
          removeFromOrigin s = (take origin s) ++ [(tail (s !! origin))] ++ (drop (origin + 1) s)
          addToDestination s = (take destination s) ++ [(crate : (s !! destination))] ++ (drop (destination + 1) s)

parseInput :: [String] -> ([Stack], [Move])
parseInput input = (parseStacks stacks, parseMoves moves)
    where (stacks, moves) = splitOn "" input

parseMoves :: [String] -> [(Int, Int, Int)]
parseMoves moves = map (parseMove) moves

parseMove :: String -> (Int, Int, Int)
parseMove move = (getInt ws 1, (getInt ws 3) - 1, (getInt ws 5) - 1)
    where ws = words move
          getInt ws idx = read (ws !! idx)

parseStacks :: [String] -> [Stack]
parseStacks input = map (trim . dropColumnId) $ filter isStack $ transpose input
    where trim = dropWhile isSpace
          dropColumnId = init
          isStack = not . isSpace . last

splitOn :: Eq a => a -> [a] -> ([a], [a])
splitOn elem list = (take index list, drop (index + 1) list)
    where index = fromMaybe 0 (elemIndex elem list)


