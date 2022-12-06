import System.IO

main = do
    contents <- readFile "input.txt"
    let datastream = head (lines contents)
    print $ findStartOfPacket datastream
    

findStartOfPacket :: String -> Int
findStartOfPacket (a:b:c:d:xs)
    | allDistinct a b c d = 4
    | otherwise = 1 + findStartOfPacket (b:c:d:xs)

allDistinct :: Char -> Char -> Char -> Char -> Bool
allDistinct a b c d = a /= b && a /= c && a /= d && b /= c && b /= d && c /= d

