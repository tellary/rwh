{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RandomSupply where

import Control.Monad             (ap)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.State       (State, get, put, runState)

class Monad m => MonadSupply s m where
  supply :: m s

data ListSupply s a = L { runListSupply :: [s] -> Maybe (a, [s]) }

instance MonadSupply s (ListSupply s) where
  supply = L $ \l -> case l of
    []     -> Nothing
    (s:ss) -> Just (s , ss)

instance Functor (ListSupply s) where
  fmap f s = do
    a <- s
    return $ f a

instance Applicative (ListSupply s) where
  pure  = return
  (<*>) = ap

instance Monad (ListSupply s) where
  return a = L $ \l -> Just (a, l)
  s >>=  f = L $ \l -> case runListSupply s l of
    Just (a,  l') -> runListSupply (f a) l'
    Nothing       -> Nothing

newtype ListStateSupply s a =
  LS (MaybeT (State [s]) a)
  deriving (Functor, Applicative, Monad)

runListStateSupply (LS m) l = case (runState . runMaybeT $ m) l of
  (Just v, l1)  -> Just (v, l1)
  (Nothing, _) -> Nothing

instance MonadSupply s (ListStateSupply s) where
  supply = LS $ MaybeT $ do
    s <- get
    case s of
      [] -> return Nothing
      (x:xs) -> do put xs
                   return $ Just x
