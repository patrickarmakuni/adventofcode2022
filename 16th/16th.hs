import System.IO
import System.Environment
import Data.Char
import Data.List
import Data.Maybe

main = do
    args <- getArgs
    let inputFile =  args !! 0
    input <- fmap lines $ readFile inputFile
    let valves = map parseValve input
        rates = map parseRate input
        tunnels = map (parseTunnels valves) input
    print valves
    print rates
    print tunnels

type Valve = Int
type Tunnel = Int
type Network = [[Tunnel]]


distance :: Valve -> Valve -> Network -> Int
distance a b network = distanceAcc [a] b network

distanceAcc :: [Valve] -> Valve -> Network -> Int
distanceAcc acc destination network
    | destination `elem` acc = 0
    | otherwise = 1 + distanceAcc (acc ++ (concat $ map (\node -> network !! node) acc)) destination network


parseTunnels :: [String] -> String -> [Tunnel]
parseTunnels valves = map valveNumber . getTunnels . drop 9 . words
    where valveNumber s = fromJust $ findIndex (== s) valves
          getTunnels = map (takeWhile isAlpha)

parseRate :: String -> Int
parseRate s = read $ dropWhile (not . isDigit) $ dropWhileEnd (not . isDigit) $ ((words s) !! 4)

parseValve :: String -> String
parseValve s = (words s) !! 1

