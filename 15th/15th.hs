import System.IO
import System.Environment
import Data.Char
import Data.List
import Data.Maybe

main = do
    args <- getArgs
    let (inputFile, rowNumber, range) = (args !! 0, read $ args !! 1, read $ args !! 2)
    input <- fmap lines $ readFile inputFile
    let pairings = map parsePairing input
    print $ countNonBeaconPositions rowNumber pairings
    print $ tuningFrequency $ findDistressBeacon range pairings


type Position = (Int, Int)
type Pairing = (Sensor, Beacon)
type Sensor = Position
type Beacon = Position
type Row = Int
type Range = (Int, Int)
type Span = (Int, Int)


countNonBeaconPositions :: Row -> [Pairing] -> Int
countNonBeaconPositions row pairings = sum $ map (countNonBeacon beaconsInRow) spans
    where beaconsInRow = map fst $ filter ((== row) . snd) $ distinct $ map snd pairings
          spans = totalSpan row pairings

countNonBeacon :: [Int] -> Span -> Int
countNonBeacon beacons (a, b) = (b + 1 - a) - beaconsInSpan
    where beaconsInSpan = length $ filter (\x -> x >= a && x <= b) beacons


tuningFrequency :: Position -> Int
tuningFrequency (x, y) = x * 4000000 + y

findDistressBeacon :: Int -> [Pairing] -> Position
findDistressBeacon range pairings = (column, row)
    where column = 1 + (minimum $ map snd rowSpans)
          rowSpans = spans !! row
          row = head $ findIndices (not . isSpanned (0, range)) spans
          spans = map (\r -> totalSpan r pairings) [0..range]


isSpanned :: Range -> [Span] -> Bool
isSpanned range spans = any (spansRange range) spans
    where spansRange (r1, r2) (s1, s2) = s1 <= r1 && s2 >= r2

totalSpan :: Row -> [Pairing] -> [Span]
totalSpan row pairings = mergeSpans $ getSpans row pairings

mergeSpans :: [Span] -> [Span]
mergeSpans [] = []
mergeSpans [x] = [x]
mergeSpans (x:xs)
    | isDistinct x xs = x : mergeSpans xs
    | otherwise       = mergeSpans $ map (tryUnion x) xs

tryUnion :: Span -> Span -> Span
tryUnion a@(a1, a2) b@(b1, b2)
    | areDistinct a b = b
    | otherwise       = (min a1 b1, max a2 b2)

isDistinct :: Span -> [Span] -> Bool
isDistinct x xs = and $ map (areDistinct x) xs

areDistinct :: Span -> Span -> Bool
areDistinct (a1, a2) (b1, b2) = a1 > b2 + 1 || b1 > a2 + 1

getSpans :: Row -> [Pairing] -> [Span]
getSpans row = concat . map (maybeToList . horizontalSpan row)

horizontalSpan :: Row -> Pairing -> Maybe Span
horizontalSpan row (sensor@(x, y), beacon)
    | yd > d = Nothing
    | otherwise = Just (x - (d - yd), x + (d - yd))
    where d = distance sensor beacon
          yd = abs (y - row)

distinct :: (Eq a) => [a] -> [a]
distinct [] = []
distinct (x:xs)
    | any (== x) xs = distinct xs
    | otherwise     = x : distinct xs

distance :: Position -> Position -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

parsePairing :: String -> Pairing
parsePairing s = (((getInt $ ws !! 2), (getInt $ ws !! 3)), ((getInt $ ws !! 8), (getInt $ ws !! 9)))
    where ws = words s
          getInt = read . reverse . dropWhile (not . isDigit) . takeWhile (/= '=') . reverse

