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
signalStrengths = ssIter 1

ssIter :: Int -> [Int] -> [Int]
ssIter _ [] = []
ssIter i (x:xs) = (i * x) : ssIter (i + 1) xs

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
drawPixels = dpIter 0

dpIter :: Int -> [Int] -> String
dpIter _ [] = []
dpIter i (x:xs) = drawChar : dpIter (i + 1) xs
    where drawChar = if spriteAligns i x then '#' else '.'

spriteAligns :: Int -> Int -> Bool
spriteAligns cycle x = abs (pixel - x) <= 1
    where pixel = cycle `mod` 40
