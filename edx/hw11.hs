last' [x] = x
last' (_:xs) = last xs

init' [_] = []
init' (x:xs) = x : init xs

[] +*+ ys = ys
(x:xs) +*+ ys = x : (xs +*+ ys)

drop1 0 xs = xs
drop1 n [] = []
drop1 n (_:xs) = drop (n-1) xs

foldl' _ v [] = v
foldl' f v (x:xs) = foldl' f (f v x) xs

foldr' _ v [] = v
foldr' f v (x:xs) = f x (foldr f v xs)


data Nat = Zero
         | Succ Nat

add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = Succ (add n m)

add n (Succ m) = Succ (add n m)


[] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

xs ++ (ys ++ zs) = (xs ++ ys) ++ zs
