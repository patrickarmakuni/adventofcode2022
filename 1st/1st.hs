import System.IO

main = do
    withFile "input.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        let inventoryList = lines contents
            inventories = splitOn "" inventoryList
            inventory1 = head inventories
        putStr $ unlines inventory1)


splitOn :: String -> [String] -> [[String]]
splitOn x ys = splitOnAcc x (reverse ys) [[]] 

splitOnAcc :: String -> [String] -> [[String]] -> [[String]]
splitOnAcc _ [] acc = acc
splitOnAcc x (y:ys) (z:zs)
    | x == y    = splitOnAcc x ys ([]:(z:zs))
    | otherwise = splitOnAcc x ys ((y:z):zs)

