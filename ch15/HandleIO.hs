{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HandleIO (HandleIO, runIO) where

import           Control.Monad.Trans (MonadIO, liftIO)
import           MonadHandle
import qualified System.IO as S

newtype HandleIO a = H { runIO :: IO a }
  deriving (Functor, Applicative, Monad)

instance MonadHandle S.Handle HandleIO where
  hClose     = H . S.hClose
  hPutStr h  = H . S.hPutStr h
  openFile h = H . S.openFile h

instance MonadIO HandleIO where
  liftIO = H
