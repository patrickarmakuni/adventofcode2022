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
part1 hmap = iterateSearch 0 [start] [start] end hmap
    where start = getPosition 'S' hmap
          end = getPosition 'E' hmap


iterateSearch :: Int -> [Position] -> [Position] -> Position -> Heightmap -> Int
iterateSearch t current searched end hmap
    | any (==end) current = t
    | otherwise = iterateSearch (t + 1) next (next ++ searched) end hmap
    where next = nub $ concat $ map (\pos -> getReachable pos searched hmap) current


getPosition :: Char -> Heightmap -> Position
getPosition c hmap = (column, row)
    where row = fromJust $ findIndex (any (== c)) hmap
          column = fromJust $ elemIndex c $ hmap !! row

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

