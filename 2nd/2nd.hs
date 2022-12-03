import System.IO

main = do
    contents <- readFile "input.txt"
    let rounds = lines contents
        tuples = map (\x -> (x !! 0, x !! 2)) rounds
        scores1 = map (\x -> calculateScore1 x) tuples
        scores2 = map (\x -> calculateScore2 x) tuples
    print $ sum scores1
    print $ sum scores2
    

calculateScore1 :: (Char, Char) -> Int
calculateScore1 (opponent, response) = (shapeScore response) + (outcomeScore opponent response)

calculateScore2 :: (Char, Char) -> Int
calculateScore2 (opponent, result) = (shapeScore response) + (outcomeScore opponent response)
    where response = requiredMove opponent result
    
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

requiredMove :: Char -> Char -> Char
requiredMove 'A' 'X' = 'Z'
requiredMove 'A' 'Y' = 'X'
requiredMove 'A' 'Z' = 'Y'
requiredMove 'B' 'X' = 'X'
requiredMove 'B' 'Y' = 'Y'
requiredMove 'B' 'Z' = 'Z'
requiredMove 'C' 'X' = 'Y'
requiredMove 'C' 'Y' = 'Z'
requiredMove 'C' 'Z' = 'X'

