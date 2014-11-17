import Data.Char

type Parser a = String -> [(a, String)]

parse :: Parser a -> String -> [(a, String)]
parse p inp = p inp

item :: Parser Char
item = \ inp ->
  case inp of
    [] -> []
    (x:xs) -> [(x,xs)]

failure :: Parser a
failure = \ inp -> []

ret :: a -> Parser a
ret v = \ inp -> [(v, inp)]

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \ inp ->
  case p inp of
    [] -> parse q inp
    [(v, out)] -> [(v, out)]


(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= f = \ inp -> case parse p inp of
  [] -> []
  [(v, out)] -> parse (f v) out

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then ret x else failure
             

digit :: Parser Char
digit = sat isDigit

char :: Char -> Parser Char
char x = sat (x ==)

p :: Parser (Char, Char)
p = do x <- item
       item
       y <- item
       (x,y)
