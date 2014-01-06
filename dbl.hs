triangle :: Int -> [(Int, Int, Int)]
triangle x = [(x, y, z) | y <- [1..x^2], z <- [1..x^2], x^2 == y^2 + z^2]

mdropwhile :: (a -> Bool) -> [a] -> [a]
mdropwhile _ [] = []
mdropwhile pred list = foldr f [] list
  where f = \e a -> if not (pred e) then (e:a)
                    else a

mydrop :: Int -> [a] -> [a]
mydrop n l =
  let
    f = \(n, xs) e -> if n > 0 then (n - 1, xs)
                        else (0, e:xs)
    (_, res) = foldl f (n, []) l
  in reverse res

mydrop' :: Int -> [a] -> [a]
mydrop' 0 xs = xs
mydrop' n (_:xs) = mydrop' (n - 1) xs
              
mdropwhile' :: (a -> Bool) -> [a] -> [a]
mdropwhile' _ [] = []
mdropwhile' f (x:xs)
  | f x = x:xs
  | otherwise = xs
