import System.IO

main = do
    withFile "input.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)

