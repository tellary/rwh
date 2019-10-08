module LazyIO where

import System.IO.Unsafe(unsafeInterleaveIO)

-- From here:
-- https://stackoverflow.com/a/12609802/1060693
lazyMapM :: (a -> IO b) -> [a] -> IO [b]
lazyMapM _ [] = return []
lazyMapM f (x:xs) = do y <- f x
                       ys <- unsafeInterleaveIO $ lazyMapM f xs
                       return (y:ys)

lazyForM = flip lazyMapM
