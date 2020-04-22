{-# LANGUAGE FlexibleInstances, FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MyState
  ( MyMonadState(..)
  , MyState
  , MyStateT
  , myRunState
  , myRunStateT
  ) where

import Control.Monad          (ap)
import Control.Monad.Identity (Identity, runIdentity)

data MyStateT s m a = S { myRunStateT :: s -> m (a, s) }

type MyState s = MyStateT s Identity

myRunState = fmap runIdentity . myRunStateT

class Monad m => MyMonadState s m | m -> s where
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
    (a, s') <- myRunStateT m s
    myRunStateT (f a) s'

instance Monad m => MyMonadState s (MyStateT s m) where
  get   = S $ \s -> return (s , s)
  put s = S $ \_ -> return ((), s)
