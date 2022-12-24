import System.IO
import System.Environment


main = do
    args <- getArgs
    let inputFile = args !! 0
    input <- fmap lines $ readFile inputFile
    let droplet = map parseCube input
    print $ surfaceArea droplet


type Cube = (Int, Int, Int)


surfaceArea :: [Cube] -> Int
surfaceArea droplet = sum $ map (\c -> exposed c droplet) droplet

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

