module HW8 where

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = putChar x >> putStr' xs

getLine' = get ""

get :: String -> IO String
get xs = do x <- getChar
            case x of
              '\n' -> return xs
              _ -> get (xs ++ [x])

putStrLn' :: String -> IO ()
putStrLn' [] = putChar '\n'
putStrLn' (x:xs) = putChar x >> putStrLn' xs

interact' :: (String -> String) -> IO ()
interact' f = do input <- getLine'
                 putStrLn' (f input)
