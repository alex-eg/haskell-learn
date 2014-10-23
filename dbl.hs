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

data Point = Point Float Float
             deriving Show
data Shape = Circle Point Float
           | Rect Point Point
             deriving Show

area :: Shape -> Float
area (Circle _ r) = pi * r^2
area (Rect (Point x1 y1) (Point x2 y2)) = abs $ (x2 - x1) * (y2 - y1)

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [x' | x' <- xs, x' >= x] ++
               [x] ++
               qsort [x' | x' <- xs, x' < x]

hello = putStrLn "Hello, world!"

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

data Tree a = Empty |
              Node a (Tree a) (Tree a)
              deriving Show

singleton  :: a -> Tree a
singleton x = Node x Empty Empty

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x Empty = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left $ treeInsert x right

fact' :: Int -> Int
fact' 0 = 1
fact' n = n * fact' (n - 1)

sumSoFar x [] = [x]
sumSoFar x (y:ys) = x : sumSoFar (x+y) ys

infiniteFib = 1 : 1 : zipWith (+) infiniteFib (tail infiniteFib)
