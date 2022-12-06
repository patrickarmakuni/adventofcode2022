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
    where processMoves stacks moves = foldl (\acc m -> sequentialMove acc m) stacks moves

part2 :: [Stack] -> [Move] -> String
part2 stacks moves = map (head) (processMoves stacks moves)
    where processMoves stacks moves = foldl (\acc m -> batchMove acc m) stacks moves


sequentialMove :: [Stack] -> Move -> [Stack]
sequentialMove stacks (0, _, _) = stacks
sequentialMove stacks (number, origin, destination) = sequentialMove (move1 stacks origin destination) (number - 1, origin, destination)

batchMove :: [Stack] -> Move -> [Stack]
batchMove stacks (number, origin, destination) = (pushTo destination crates . popFrom origin number) stacks
    where crates = take number (stacks !! origin)

move1 :: [Stack] -> Int -> Int -> [Stack]
move1 stacks origin destination = (pushTo destination crates . popFrom origin 1) stacks
    where crates = [head (stacks !! origin)]

popFrom :: Int -> Int -> [Stack] -> [Stack]
popFrom idx number stacks = (take idx stacks) ++ [(drop number (stacks !! idx))] ++ (drop (idx + 1) stacks)

pushTo :: Int -> [Char] -> [Stack] -> [Stack]
pushTo idx crates stacks = (take idx stacks) ++ [(crates ++ (stacks !! idx))] ++ (drop (idx + 1) stacks)

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


