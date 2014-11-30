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

mM1  :: Monad m => (a -> m b) -> [a] -> m [b]
mM2  :: Monad m => (a -> m b) -> [a] -> m [b]
mM6  :: Monad m => (a -> m b) -> [a] -> m [b]
mM7  :: Monad m => (a -> m b) -> [a] -> m [b]
mM8  :: Monad m => (a -> m b) -> [a] -> m [b]

mM1 f as = slist1 (map f as)

mM2 f [] = return []
mM2 f (a : as)
  = f a >>= \b -> mM2 f as >>= \bs -> return (b : bs)

mM6 f [] = return []
mM6 f (a : as) =
  do b <- f a
     bs <- mM6 f as
     return (b : bs)

mM7 f [] = return []
mM7 f (a : as)
  = f a >>=
    \ b ->
      do bs <- mM7 f as
         return (b : bs)

mM8 f [] = return []
mM8 f (a : as)
  = f a >>=
    \ b ->
      do bs <- mM8 f as
         return (bs ++ [b])

filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM _ [] = return []
filterM p (x : xs) = do flag <- p x
                        ys <- filterM p xs
                        if flag then return (x:ys) else return ys

foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldLeftM f a [] = return a
foldLeftM f a (x:xs) = f a x >>= (\ n -> foldLeftM f n xs)

foldRightM :: Monad m => (b -> a -> m a) -> a -> [b] -> m a
foldRightM f a xs = foldr (\x a -> a >>= (\b -> f x b)) (return a) xs
