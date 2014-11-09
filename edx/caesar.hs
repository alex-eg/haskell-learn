import Data.Char

let2int :: Char -> Int
let2int c = ord c - ord 'A'

int2let :: Int -> Char
int2let n = chr (n + ord 'A')

shift :: Int -> Char -> Char
shift n c
    | isUpper c = int2let ((let2int c + n) `mod` 26)
    | isLower c = int2let ((let2int c - let2int 'a' + n) `mod` 26 + let2int 'a')
    | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
