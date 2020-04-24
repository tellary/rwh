{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

import Control.Exception    (ArithException (DivideByZero), throw)
import Control.Monad        (when)
import Control.Monad.Except (MonadError (throwError))

divBy :: Integral a => a -> [a] -> Either ArithException [a]
divBy nominator []     = Right []
divBy nominator (0:_)  = Left DivideByZero
divBy nominator (x:xs) = case divBy nominator xs of
  Right xs' -> Right $ nominator `div` x:xs'
  l         -> l

divByEx
  :: (Integral a, Monad m)
  => a -> [a] -> m [a]
divByEx nominator []     = return []
divByEx nominator (x:xs) = do
  when (x == 0) $ throw DivideByZero
  ((nominator `div` x) : ) <$> divByEx nominator xs

-- This isn't very useful generalization, because the only instance of
-- `MonadError ArithException m` would be `Either ArithException`
divByM
  :: (Integral a, MonadError ArithException m)
  => a -> [a] -> m [a]
divByM nominator []     = return []
divByM nominator (x:xs) = do
  when (x == 0) $ throwError DivideByZero
  ((nominator `div` x) : ) <$> divByM nominator xs

divByLazy nominator xs
  = map
    (\case 0 -> Left DivideByZero
           x -> Right $ nominator `div` x)
    xs

divBySeq n = sequence . divByLazy n
