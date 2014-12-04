data Tree a = Leaf a
            | Node (Tree a) a (Tree a) 
            | Bottom deriving Show

add :: (Ord a) => Tree a -> a -> Tree a
add (Leaf n) m = if m > n then Node Bottom n (Leaf m)
                 else Node (Leaf m) n Bottom
add (Node l n r) m = if m > n then (Node l n (add r m))
                     else (Node (add l m) n r)
add Bottom m = Leaf m

data MyBool = True' | False' | Maybe' deriving Show

flip' :: MyBool -> MyBool
flip' True' = False'
flip' False' = True'
flip' Maybe' = Maybe'
