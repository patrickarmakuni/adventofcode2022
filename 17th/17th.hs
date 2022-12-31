import System.IO
import System.Environment
import Data.List


main = do
    args <- getArgs
    let inputFile = args !! 0
    input <- fmap init $ readFile inputFile
    print $ part1 input


type Position = (Int, Int)
type Rock = [Position]
type Structure = [Position]
type Jet = Char


part1 :: [Jet] -> Structure
part1 jets = fall [(2, 4), (3, 4), (4, 4), (5, 4)] jets []


fall :: Rock -> [Jet] -> Structure -> Structure
fall rock (jet:jets) structure 
    | isBlocked rock structure = rock ++ structure
    | otherwise = fall (fallCycle jet rock) (jets ++ [jet]) structure

fallCycle :: Jet -> Rock -> Rock
fallCycle jet = pushOne jet . dropOne
    where dropOne = map (\(x, y) -> (x, y - 1))

pushOne :: Jet -> Rock -> Rock
pushOne '>' = pushRight
pushOne '<' = pushLeft

pushRight :: Rock -> Rock
pushRight rock
    | any (\(x, _) -> x == 6) rock = rock
    | otherwise = map (\(x, y) -> (x + 1, y)) rock

pushLeft :: Rock -> Rock
pushLeft rock
    | any (\(x, _) -> x == 0) rock = rock
    | otherwise = map (\(x, y) -> (x - 1, y)) rock

isBlocked :: Rock -> Structure -> Bool
isBlocked rock structure = any isAtRest rock
    where isAtRest (x1, y1) = y1 == 0 || any (\(x2, y2) -> x1 == x2 && y1 == y2 + 1) structure

