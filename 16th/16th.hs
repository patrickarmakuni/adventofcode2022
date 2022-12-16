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


distance :: Valve -> Valve -> Network -> [Valve] -> Int
distance a b network visited = (length $ shortestPath a b network) - 1

shortestPath :: Valve -> Valve -> Network -> [Valve]
shortestPath a b network = reverse $ shortestPathAcc a b network [a]

shortestPathAcc :: Valve -> Valve -> Network -> [Valve] -> [Valve]
shortestPathAcc a b network visited
    | any (== b) paths = b : visited
    | otherwise = case untriedPaths of [] -> []
                                       ps -> shortest $ map (\path -> shortestPathAcc path b network (path : visited)) ps
    where untriedPaths = filter (\p -> not $ any (== p) visited) $ paths
          shortest = foldl1 (\acc x -> if length x < length acc && length x > 0 then x else acc)
          paths = network !! a


parseTunnels :: [String] -> String -> [Tunnel]
parseTunnels valves = map valveNumber . getTunnels . drop 9 . words
    where valveNumber s = fromJust $ findIndex (== s) valves
          getTunnels = map (takeWhile isAlpha)

parseRate :: String -> Int
parseRate s = read $ dropWhile (not . isDigit) $ dropWhileEnd (not . isDigit) $ ((words s) !! 4)

parseValve :: String -> String
parseValve s = (words s) !! 1

