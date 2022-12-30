import System.IO
import System.Environment
import Data.List
import Data.Maybe


main = do
    args <- getArgs
    let inputFile = args !! 0
    input <- fmap lines $ readFile inputFile
    print $ part1 input


type Position = (Int, Int)
type Heightmap = [String]


part1 :: Heightmap -> Int
part1 hmap = shortestPath start end [] hmap
    where start = getPosition 'S' hmap
          end = getPosition 'E' hmap


getPosition :: Char -> Heightmap -> Position
getPosition c hmap = (column, row)
    where row = fromJust $ findIndex (any (== c)) hmap
          column = fromJust $ elemIndex c $ hmap !! row

shortestPath :: Position -> Position -> [Position] -> Heightmap -> Int
shortestPath a b visited hmap
    | a == b = 0
    | length reachable == 0 = 1000000
    | otherwise = (+1) $ minimum $ map (\c -> shortestPath c b (a : visited) hmap) reachable
    where reachable = getReachable a visited hmap

getReachable :: Position -> [Position] -> Heightmap -> [Position]
getReachable pos@(x, y) visited hmap = filter (not . tooHigh hmap pos) $ filter isNotVisited $ filter (isOnMap hmap) neighbours
    where neighbours = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
          isNotVisited pos = not $ any (== pos) visited

tooHigh :: Heightmap -> Position -> Position -> Bool
tooHigh hmap current neighbour = height neighbour > height current + 1
    where height (x, y) = heightValue $ (hmap !! y) !! x

heightValue :: Char -> Int
heightValue 'S' = heightValue 'a'
heightValue 'E' = heightValue 'z'
heightValue c = fromJust $ elemIndex c ['a'..'z']

isOnMap :: Heightmap -> Position -> Bool
isOnMap hmap (x, y) = x >= 0 && y >= 0 && x < (width hmap) && y < (height hmap)
    where width hmap = length (head hmap)
          height hmap = length hmap

