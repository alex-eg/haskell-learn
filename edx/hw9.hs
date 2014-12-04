import Data.List
import Data.Char
import Unsafe.Coerce

data Nat = Zero
         | Succ Nat
         deriving Show

nat2int :: Nat -> Int
nat2int = head . m
  where m Zero = [0]
        m (Succ n) = [Data.List.sum [x | x <- (1 : m n)]]

-- int2nat :: Int -> Nat
-- int2nat (n+1) = Succ (int2nat n)
-- int2nat 0 = Zero

add :: Nat -> Nat -> Nat
add n (Succ m) = Succ (add m n)
add n Zero = n

mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult m (Succ n) = add m (mult m n)

data Tree = Leaf Integer
          | Node Tree Integer Tree

occurs m (Leaf n) = m == n
occurs m (Node l n r)
  = case compare n m of
  LT -> occurs m l
  EQ -> True
  GT -> occurs m r

class Monoid a where
  mempty :: a
  (<>) :: a -> a -> a

instance Monoid [a] where
  mempty = []
  (<>) = (++)
