{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
module MyExceptT
  ( MyExcept
  , MyExceptT(..)
  , mapMyExceptT
  , runMyExcept
  ) where

import Control.Monad          (ap)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader   (MonadReader (ask, local))
import Control.Monad.Trans    (MonadTrans (lift))
import Control.Monad.State    (MonadState (get, put))
import Control.Monad.Writer   (MonadWriter (listen, pass, tell))

newtype MyExceptT e m a = MyExceptT { runMyExceptT :: m (Either e a) }

type MyExcept e = MyExceptT e Identity
runMyExcept = runIdentity . runMyExceptT

instance Monad m => Functor (MyExceptT e m) where
  fmap f m = m >>= \a -> return $ f a

instance Monad m => Applicative (MyExceptT e m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (MyExceptT e m) where
  return a = MyExceptT . return . Right $ a
  m >>= f = MyExceptT $ do
    eth <- runMyExceptT m
    case eth of
      Right a -> runMyExceptT $ f a
      Left  e -> return $ Left e

instance MonadTrans (MyExceptT e) where
  lift m = MyExceptT $ m >>= \a -> return . Right $ a

mapMyExceptT
  :: (m (Either e1 a) -> n (Either e2 b))
  -> MyExceptT e1 m a
  -> MyExceptT e2 n b
mapMyExceptT f = MyExceptT . f . runMyExceptT

instance MonadReader r m => MonadReader r (MyExceptT e m) where
  ask     = lift ask
  local f = mapMyExceptT $ local f

extendEitherTuple
  :: MonadWriter w m
  => m (Either e a, w) -> m (Either e (a, w))
extendEitherTuple = fmap (\(eth, w) -> fmap (, w) eth)

instance MonadWriter w m => MonadWriter w (MyExceptT e m) where
  tell   = lift . tell
  listen = mapMyExceptT $ extendEitherTuple . listen
  pass   = mapMyExceptT $ \m -> pass $ do
    e <- m
    return $ case e of
      Right (a, f) -> (Right a, f)
      Left  e      -> (Left  e, id)

instance MonadState s m => MonadState s (MyExceptT e m) where
  get = lift get
  put = lift . put

instance MonadIO m => MonadIO (MyExceptT e m) where
  liftIO = lift . liftIO
