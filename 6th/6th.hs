import System.IO

main = do
    contents <- readFile "input.txt"
    let datastream = head (lines contents)
    print $ findStartOfPacket datastream
    

findStartOfPacket :: String -> Int
findStartOfPacket (a:b:c:d:xs)
    | distinct [a,b,c,d] = 4
    | otherwise = 1 + findStartOfPacket (b:c:d:xs)

distinct :: (Eq a) => [a] -> Bool
distinct [] = True
distinct (x:[]) = True
distinct (x:xs)
    | any (== x) xs = False
    | otherwise = distinct xs

