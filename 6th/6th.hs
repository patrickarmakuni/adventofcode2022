import System.IO

main = do
    contents <- readFile "input.txt"
    let datastream = head (lines contents)
    print $ findFirstDistinctSequence datastream 4
    print $ findFirstDistinctSequence datastream 14
    

findFirstDistinctSequence :: String -> Int -> Int
findFirstDistinctSequence all@(x:xs) seqLength
    | length all < seqLength = -1
    | distinct (take seqLength all) = seqLength
    | otherwise = 1 + (findFirstDistinctSequence xs seqLength)

distinct :: (Eq a) => [a] -> Bool
distinct [] = True
distinct (x:[]) = True
distinct (x:xs)
    | any (== x) xs = False
    | otherwise = distinct xs

