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


parseTunnels :: [String] -> String -> [Int]
parseTunnels valves = map valveNumber . getTunnels . drop 9 . words
    where valveNumber s = fromJust $ findIndex (== s) valves
          getTunnels = map (takeWhile isAlpha)

parseRate :: String -> Int
parseRate s = read $ dropWhile (not . isDigit) $ dropWhileEnd (not . isDigit) $ ((words s) !! 4)

parseValve :: String -> String
parseValve s = (words s) !! 1

