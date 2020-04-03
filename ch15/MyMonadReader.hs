{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Monad (ap)

class Monad m => MyMonadReader a m where
  ask :: m a

newtype MyReaderT a m b = R { runMyReaderT :: a -> m b }

instance Monad m => Functor (MyReaderT a m) where
  fmap f m = m >>= \v -> return $ f v

instance Monad m => Applicative (MyReaderT a m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (MyReaderT a m) where
  return b = R $ \_ -> return b
  m >>= f = R $ \a -> do
    v <- runMyReaderT m a
    runMyReaderT (f v) a

instance Monad m => MyMonadReader a (MyReaderT a m) where
  ask = R $ \a -> return a

-- λ> runMyReaderT ((*) <$> pure 2 <*> ask) 2
-- 4
-- λ> runMyReaderT (do { v1 <- ask; v2 <- ask; return $ v1^v2}) 3
-- 27
