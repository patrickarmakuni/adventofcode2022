import System.IO
import System.Environment
import Data.Char
import Data.List
import Data.Maybe

main = do
    args <- getArgs
    let (inputFile, parameter) = (args !! 0, read $ args !! 1)
    input <- fmap lines $ readFile inputFile
    let pairings = map parsePairing input
    print $ countNonBeaconPositions parameter pairings
    print $ findDistressBeacon parameter pairings


type Position = (Int, Int)
type Pairing = (Sensor, Beacon)
type Sensor = Position
type Beacon = Position
type Row = Int
type Range = (Int, Int)

findDistressBeacon :: Int -> [Pairing] -> Position
findDistressBeacon range pairings = fromMaybe (0, 0) $ find isDistressBeacon allPositions
    where allPositions = (,) <$> [0..range] <*> [0..range]
          isDistressBeacon pos = not $ any (isWithinRangeOf pos) pairings

countNonBeaconPositions :: Row -> [Pairing] -> Int
countNonBeaconPositions row pairings = length $ filter (isNonBeacon pairings) $ map (\x -> (x, row)) [xmin..xmax]
    where (xmin, xmax) = totalHorizontalRange pairings

isNonBeacon :: [Pairing] -> Position -> Bool
isNonBeacon pairings pos = (not . any (== pos)) beacons && any (isWithinRangeOf pos) pairings
    where beacons = map snd pairings

totalHorizontalRange :: [Pairing] -> Range
totalHorizontalRange = foldl1 (\(xmin, ymin) (x, y) -> (min x xmin, max y ymin)) . map horizontalRange

horizontalRange :: Pairing -> Range
horizontalRange (sensor@(x, _), beacon) = (x - d, x + d)
    where d = distance sensor beacon

isWithinRangeOf :: Position -> Pairing -> Bool
isWithinRangeOf pos (sensor, beacon) = (distance pos sensor) <= (distance sensor beacon)

distance :: Position -> Position -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

parsePairing :: String -> Pairing
parsePairing s = (((getInt $ ws !! 2), (getInt $ ws !! 3)), ((getInt $ ws !! 8), (getInt $ ws !! 9)))
    where ws = words s
          getInt = read . reverse . dropWhile (not . isDigit) . takeWhile (/= '=') . reverse

