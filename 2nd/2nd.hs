import System.IO

main = do
    contents <- readFile "input.txt"
    let rounds = lines contents
        tuples = map (\x -> (x !! 0, x !! 2)) rounds
        scores = map (\x -> calculateScore x) tuples
    print (sum scores)
    

calculateScore :: (Char, Char) -> Int
calculateScore (opponent, response) = (shapeScore response) + (outcomeScore opponent response)
    
shapeScore :: Char -> Int
shapeScore 'X' = 1
shapeScore 'Y' = 2
shapeScore 'Z' = 3

outcomeScore :: Char -> Char -> Int
outcomeScore 'A' 'Z' = 0
outcomeScore 'B' 'X' = 0
outcomeScore 'C' 'Y' = 0
outcomeScore 'A' 'X' = 3
outcomeScore 'B' 'Y' = 3
outcomeScore 'C' 'Z' = 3
outcomeScore 'A' 'Y' = 6
outcomeScore 'B' 'Z' = 6
outcomeScore 'C' 'X' = 6

