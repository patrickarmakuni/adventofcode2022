import System.IO
import System.Environment


main = do
    args <- getArgs
    let inputFile = args !! 0
    input <- fmap lines $ readFile inputFile
    print $ part1 input


part1 :: [String] -> String
part1 = toSnafu "" . sum . map fromSnafu


toSnafu :: String -> Int -> String
toSnafu acc 0 = acc
toSnafu acc n
    | remainder < 3 = toSnafu ((toSnafuDigit remainder) : acc) (n `div` 5)
    | otherwise     = toSnafu ((toSnafuDigit remainder) : acc) ((n + 5) `div` 5)
    where remainder = n `mod` 5

toSnafuDigit :: Int -> Char
toSnafuDigit 0 = '0'
toSnafuDigit 1 = '1'
toSnafuDigit 2 = '2'
toSnafuDigit 3 = '='
toSnafuDigit 4 = '-'


fromSnafu :: String -> Int
fromSnafu = sum . zipWith (*) powersOfFive . map (fromSnafuDigit) . reverse
    where powersOfFive = zipWith (^) (repeat 5) [0..]

fromSnafuDigit :: Char -> Int
fromSnafuDigit '=' = -2
fromSnafuDigit '-' = -1
fromSnafuDigit '0' = 0
fromSnafuDigit '1' = 1
fromSnafuDigit '2' = 2

