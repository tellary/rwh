module Logger (Logger, Log, addLog) where

import Control.Applicative (liftA2)
import Control.Monad       (liftM, liftM2)

type Log = [String]

newtype Logger a = Logger (a, Log) deriving Show

instance Functor Logger where
  fmap = liftM

instance Applicative Logger where
  pure   = return
  liftA2 = liftM2

instance Monad Logger where
  return a                = Logger (a, [])
  (Logger (a, log)) >>= f = Logger (a', foldr (:) log log')
    where Logger (a', log') = f a

addLog msg = Logger (Nothing, [msg])
