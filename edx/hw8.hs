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

sequence_' :: Monad m => [m a] -> m ()
sequence_' [] = return ()
sequence_' (m:ms) = (foldl (>>) m ms) >> return ()

sequence_2' :: Monad m => [m a] -> m ()
sequence_2' [] = return ()
sequence_2' (m:ms) = m >> sequence_2' ms

sequence_3' :: Monad m => [m a] -> m ()
sequence_3' [] = return ()
sequence_3' (m:ms) = m >>= \_ -> sequence_3' ms

sequence_4' :: Monad m => [m a] -> m ()
sequence_4' ms = foldr (>>) (return()) ms

slist1 :: Monad m => [m a] -> m [a]
slist5 :: Monad m => [m a] -> m [a]
slist8 :: Monad m => [m a] -> m [a]

slist1 [] = return []
slist1 (m:ms) = m >>= \ a -> do as <- slist1 ms
                                return (a:as)
slist5 ms = foldr f (return []) ms
  where
    f :: (Monad m) => m a -> m [a] -> m [a]
    f m acc = do x <- m
                 xs <- acc
                 return (x:xs)

slist8 [] = return []
slist8 (m:ms)
  = do a <- m
       as <- slist8 ms
       return (a:as)
