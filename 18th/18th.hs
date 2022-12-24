import System.IO
import System.Environment


main = do
    args <- getArgs
    let inputFile = args !! 0
    input <- fmap lines $ readFile inputFile
    let droplet = map parseCube input
    print $ part1 droplet
    print $ part2 droplet


type Cube = (Int, Int, Int)
type Bounds1 = (Int, Int)
type Bounds3 = (Bounds1, Bounds1, Bounds1)


part2 :: [Cube] -> Int
part2 droplet = sum $ map (\c -> exposed' c droplet) droplet

exposed' :: Cube -> [Cube] -> Int
exposed' cube droplet = length $ filter (isFreeAir b droplet) $ neighbours cube
    where b = bounds droplet

isFreeAir :: Bounds3 -> [Cube] -> Cube -> Bool
isFreeAir bounds cubes cube
    | isOutOfBounds cube bounds = True
    | cubes `contains` cube = False
    | otherwise = any (isFreeAir bounds cubes) $ neighbours cube

isOutOfBounds :: Cube -> Bounds3 -> Bool
isOutOfBounds (x, y, z) ((x0, x1), (y0, y1), (z0, z1)) =
                x < x0 || x > x1 ||
                y < y0 || y > y1 ||
                z < z0 || z > z1

neighbours :: Cube -> [Cube]
neighbours (x, y, z) = [(x + 1, y, z), (x - 1, y, z),
                        (x, y + 1, z), (x, y - 1, z),
                        (x, y, z + 1), (x, y, z - 1)]

bounds :: [Cube] -> Bounds3
bounds droplet = (\(x, y, z) -> (b x, b y, b z)) $ unzip3 droplet
    where b xs = (minimum xs, maximum xs)


part1 :: [Cube] -> Int
part1 droplet = sum $ map (\c -> exposed c droplet) droplet

exposed :: Cube -> [Cube] -> Int
exposed cube others = 6 - countAdjacent cube others

countAdjacent :: Cube -> [Cube] -> Int
countAdjacent cube others = foldl (\acc x -> if x `isAdjacentTo` cube then acc + 1 else acc) 0 others

isAdjacentTo :: Cube -> Cube -> Bool
(x1, y1, z1) `isAdjacentTo` (x2, y2, z2) =
    x1 == x2 && y1 == y2 && abs (z1 - z2) == 1 ||
    x1 == x2 && z1 == z2 && abs (y1 - y2) == 1 ||
    y1 == y2 && z1 == z2 && abs (x1 - x2) == 1

parseCube :: String -> Cube
parseCube s = (getInt 0, getInt 1, getInt 2)
    where getInt x = read $ ws !! x
          ws = wordsWhen (==',') s

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                     "" -> []
                     s' -> w : wordsWhen p s''
                        where (w, s'') = break p s'

contains :: (Eq a) => [a] -> a -> Bool
xs `contains` x = any (== x) xs

