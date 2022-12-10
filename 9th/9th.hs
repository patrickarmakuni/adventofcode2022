import System.IO

main = do
    contents <- readFile "input.txt"
    let instructions = lines contents
    print $ part1 instructions
    putStr $ part2 instructions


part1 :: [String] -> Int
part1 instructions = sum $ map (ss !!) [19,59..219]
    where ss = signalStrengths $ getStates instructions

signalStrengths :: [Int] -> [Int]
signalStrengths = iter (*) 1

getStates :: [String] -> [Int]
getStates = reverse . foldl (updateState) [1]

updateState :: [Int] -> String -> [Int]
updateState (x:xs) "noop" = x:x:xs
updateState (x:xs) instruction = addx:x:x:xs
    where addx = x + (read . last . words) instruction


part2 :: [String] -> String
part2 = breakIntoLines . drawPixels . getStates

breakIntoLines :: String -> String
breakIntoLines [] = []
breakIntoLines s = a ++ "\n" ++ (breakIntoLines b)
    where (a, b) = splitAt 40 s

drawPixels :: [Int] -> String
drawPixels = iter (\i x -> if spriteAligns i x then '#' else '.') 0

spriteAligns :: Int -> Int -> Bool
spriteAligns cycle x = abs (pixel - x) <= 1
    where pixel = cycle `mod` 40


iter :: (Int -> a -> b) -> Int -> [a] -> [b]
iter _ _ [] = []
iter f i (x:xs) = (f i x) : iter f (i + 1) xs

