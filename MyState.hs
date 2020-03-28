{-# LANGUAGE FlexibleInstances, FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MyState where

import Control.Monad          (ap)
import Control.Monad.Identity (Identity)

data MyStateT s m a = S { myRunState :: s -> m (a, s) }

type MyState s = MyStateT s Identity

class MyMonadState s m | m -> s where
  get :: m s
  put :: s -> m ()

instance Monad m => Functor (MyStateT s m) where
  fmap f s = do
    a <- s
    return $ f a

instance Monad m => Applicative (MyStateT s m) where
  pure = return
  (<*>) = ap
  
instance Monad m => Monad (MyStateT s m) where
  return a = S $ \s -> return (a, s)
  m >>= f = S $ \s -> do
    (a, s') <- myRunState m s
    myRunState (f a) s'

instance Monad m => MyMonadState s (MyStateT s m) where
  get   = S $ \s -> return (s , s)
  put s = S $ \_ -> return ((), s)

-- λ> myRunState ((+) <$> pure 1 <*> pure 2) 0
-- (3,0)
-- λ> myRunState (fmap (*3) $ return 2) 0
-- (6,0)

s1 :: MyState Int Int
s1 = do
  v <- (+) <$> pure 1 <*> pure 2
  s <- get
  put (s*2)
  return v

-- λ> myRunState s1 1
-- Identity (3,2)
