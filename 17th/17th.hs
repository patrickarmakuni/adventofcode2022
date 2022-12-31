import System.IO
import System.Environment
import Data.List


main = do
    args <- getArgs
    let (inputFile, n) = (args !! 0, read $ args !! 1)
    input <- fmap init $ readFile inputFile
    print $ runFor n input


type Position = (Int, Int)
type Rock = [Position]
type Structure = [Position]
type Jet = Char


runFor :: Int -> [Jet] -> Int
runFor n jets = (+1) $ topOf $ loop 0 n jets base


loop :: Int -> Int -> [Jet] -> Structure -> Structure
loop i n jets structure
    | i == n = structure
    | otherwise = loop (i + 1) n newJets newStructure
    where (newStructure, newJets) = fall (getRock (i `mod` 5) ((topOf structure) + 5)) jets structure

topOf :: Structure -> Int
topOf structure = snd $ maximumBy (\(_, y1) (_, y2) -> compare y1 y2) structure


fall :: Rock -> [Jet] -> Structure -> (Structure, [Jet])
fall rock all@(jet:jets) structure 
    | isBlockedDown rock structure = (rock ++ structure, all)
    | otherwise = fall (fallCycle jet structure rock) (jets ++ [jet]) structure

fallCycle :: Jet -> Structure -> Rock -> Rock
fallCycle jet structure = pushOne jet structure . dropOne
    where dropOne = map (\(x, y) -> (x, y - 1))

pushOne :: Jet -> Structure -> Rock -> Rock
pushOne '>' structure = pushRight structure
pushOne '<' structure = pushLeft structure

pushLeft :: Structure -> Rock -> Rock
pushLeft structure rock
    | isBlockedLeft rock structure = rock
    | otherwise = map (\(x, y) -> (x - 1, y)) rock

pushRight :: Structure -> Rock -> Rock
pushRight structure rock
    | isBlockedRight rock structure = rock
    | otherwise = map (\(x, y) -> (x + 1, y)) rock


isBlockedLeft :: Rock -> Structure -> Bool
isBlockedLeft rock structure = any ibl rock
    where ibl (x1, y1) = x1 == 0 || any (\(x2, y2) -> x1 == x2 + 1 && y1 == y2) structure

isBlockedRight :: Rock -> Structure -> Bool
isBlockedRight rock structure = any ibr rock
    where ibr (x1, y1) = x1 == 6 || any (\(x2, y2) -> x1 == x2 - 1 && y1 == y2) structure

isBlockedDown :: Rock -> Structure -> Bool
isBlockedDown rock structure = any ibd rock
    where ibd (x1, y1) = y1 == 0 || any (\(x2, y2) -> x1 == x2 && y1 == y2 + 1) structure


getRock :: Int -> Int -> Rock
getRock i = case i of 0 -> one
                      1 -> two
                      2 -> three
                      3 -> four
                      4 -> five

one :: Int -> Rock
one y = [(2, y), (3, y), (4, y), (5, y)]

two :: Int -> Rock
two y = [(2, y + 1), (3, y), (3, y + 1), (3, y + 2), (4, y + 1)]

three :: Int -> Rock
three y = [(2, y), (3, y), (4, y), (4, y + 1), (4, y + 2)]

four :: Int -> Rock
four y = [(2, y), (2, y + 1), (2, y + 2), (2, y + 3)]

five :: Int -> Rock
five y = [(2, y), (2, y + 1), (3, y), (3, y + 1)]

base :: Rock
base = [(0, -1), (1, -1), (2, -1), (3, -1), (4, -1), (5, -1), (6, -1)]


