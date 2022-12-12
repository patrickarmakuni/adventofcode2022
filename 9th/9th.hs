import System.IO

main = do
    contents <- readFile "testInput.txt"
    let input = lines contents
    print $ convert input


type Direction = Char

convert :: [String] -> [Direction]
convert = concat . map expand

expand :: String -> [Direction]
expand s = replicate n c
    where c = head s
          n = read $ last $ words s

