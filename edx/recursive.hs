pow :: Int -> Int -> Int
pow a 0 = 1
pow a b = a * (pow a (b - 1))

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) =
    if x <= y then x : merge xs (y:ys)
    else y : merge (x:xs) ys

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort xs = merge (msort ys) (msort zs)
    where (ys, zs) = halve xs
