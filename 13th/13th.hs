import System.IO
import Data.Char

main = do
    contents <- readFile "testInput.txt"
    let pairs = splitOn "" $ lines contents
    print $ part1 pairs


type Pair = [String]

part1 :: [Pair] -> [Ordering]
part1 pairs = map (\ pair -> readPacket (pair !! 0) `compare` readPacket (pair !! 1)) pairs


data Packet = Value Int | List [Packet] deriving (Eq)

instance Ord Packet where
    Value a `compare` Value b = a `compare` b
    List a `compare` List b = a `compare` b
    Value a `compare` List b = List [Value a] `compare` List b
    List a `compare` Value b = List a `compare` List [Value b]

instance Show Packet where
    show (Value a) = show a
    show (List a) = show a

readPacket :: String -> Packet
readPacket s
    | head s == '[' = List (map readPacket $ getElems $ (init . tail) s)
    | otherwise = Value (read s)

getElems :: String -> [String]
getElems "" = []
getElems s@(x:xs)
    | x == ',' = getElems xs
    | x == '[' = sublist : getElems rest
    | isDigit x = (takeWhile isDigit s) : (getElems $ dropWhile isDigit s)
    where (sublist, rest) = splitAt (findMatchingBrace 0 s) s

findMatchingBrace :: Int -> String -> Int
findMatchingBrace level "" = 0
findMatchingBrace level (x:xs)
    | x == '[' = 1 + findMatchingBrace (level + 1) xs
    | x == ']' = if level == 1 then 1 else 1 + findMatchingBrace (level - 1) xs
    | otherwise = 1 + findMatchingBrace level xs



splitOn :: String -> [String] -> [[String]]
splitOn x ys = splitOnAcc x (reverse ys) [[]] 

splitOnAcc :: String -> [String] -> [[String]] -> [[String]]
splitOnAcc _ [] acc = acc
splitOnAcc x (y:ys) (z:zs)
    | x == y    = splitOnAcc x ys ([]:(z:zs))
    | otherwise = splitOnAcc x ys ((y:z):zs)

