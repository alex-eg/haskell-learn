double x = x + x
quadruple x = double (double x)

factorial n = product [1..n]
average ns = sum ns `div` length ns

n = a `div` length xs
    where
      a = 10
      xs = [1,2,3,4,5]

qsort1 [] = []
qsort1 (x : xs) = qsort1 larger ++ [x] ++ qsort1 smaller
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

qsort2 [] = []
qsort2 (x : xs) = reverse (qsort2 larger ++ [x] ++ qsort2 smaller)
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

qsort3 [] = []
qsort3 (x : xs) = qsort3 larger ++ qsort3 smaller ++ [x]
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

qsort4 [] = []
qsort4 (x : xs) = reverse (qsort4 smaller) ++ [x] ++ reverse (qsort4 larger)
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

qsort5 [] = []
qsort5 (x : xs) = qsort5 larger ++ [x] ++ qsort5 smaller
  where smaller = [a | a <- xs, a > x || a == x]
        larger = [b | b <- xs, b < x]

qsort7 [] = []
qsort7 (x : xs) = reverse
                  (reverse (qsort7 larger) ++ [x] ++ reverse (qsort7 smaller))
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

qsort8 [] = []
qsort8 (x : xs) = x : qsort8 larger ++  qsort8 smaller
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]
