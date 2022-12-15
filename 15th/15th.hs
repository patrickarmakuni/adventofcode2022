import System.IO
import System.Environment
import Data.Char

main = do
    args <- getArgs
    let (inputFile, rowNumber) = (args !! 0, read $ args !! 1)
    input <- fmap lines $ readFile inputFile
    let pairings = map parsePairing input
    print $ countNonBeaconPositions rowNumber pairings


type Position = (Int, Int)
type Pairing = (Sensor, Beacon)
type Sensor = Position
type Beacon = Position
type Row = Int
type Range = (Int, Int)

countNonBeaconPositions :: Row -> [Pairing] -> Int
countNonBeaconPositions row pairings = length $ filter (isNonBeacon pairings) $ map (\x -> (x, row)) [left..right]
    where (left, right) = totalHorizontalRange pairings

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

