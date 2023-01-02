import System.IO
import System.Environment
import Data.List
import Data.Maybe


main = do
    args <- getArgs
    let inputFile = args !! 0
    input <- fmap lines $ readFile inputFile
    print $ part2 input


type Position = (Int, Int)
type Heightmap = [String]


part1 :: Heightmap -> Int
part1 hmap = iterateSearch 0 [start] [start] end hmap
    where start = getPosition 'S' hmap
          end = getPosition 'E' hmap


part2 :: Heightmap -> Int
part2 hmap = iterateStartPositions (0, 0) hmap 1000000


iterateStartPositions :: Position -> Heightmap -> Int -> Int
iterateStartPositions pos@(x, y) hmap acc
    | x + 1 < width = iterateStartPositions (x + 1, y) hmap getShortest
    | y + 1 < height = iterateStartPositions (0, y + 1) hmap getShortest
    | otherwise = acc
    where getShortest = if heightAt pos hmap > 0 then acc else min acc $ shortestPath
          shortestPath = iterateSearch 0 [pos] [pos] end hmap
          end = getPosition 'E' hmap
          (width, height) = dimensions hmap


iterateSearch :: Int -> [Position] -> [Position] -> Position -> Heightmap -> Int
iterateSearch _ [] _ _ _ = 1000000
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

heightAt :: Position -> Heightmap -> Int
heightAt (x, y) hmap = heightValue $ (hmap !! y) !! x

heightValue :: Char -> Int
heightValue 'S' = heightValue 'a'
heightValue 'E' = heightValue 'z'
heightValue c = fromJust $ elemIndex c ['a'..'z']

isOnMap :: Heightmap -> Position -> Bool
isOnMap hmap (x, y) = x >= 0 && y >= 0 && x < width && y < height
    where (width, height) = dimensions hmap

dimensions :: Heightmap -> (Int, Int)
dimensions hmap = (width, height)
    where width = length (head hmap)
          height = length hmap

