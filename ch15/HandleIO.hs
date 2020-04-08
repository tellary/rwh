{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HandleIO (HandleIO, runIO) where

import           Control.Monad.Catch ( MonadCatch
                                     , MonadMask
                                     , MonadThrow(..))
import           Control.Monad.Trans (MonadIO)
import           MonadHandle
import qualified System.IO as S

newtype HandleIO a = H { runIO :: IO a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadIO)

instance MonadHandle S.Handle HandleIO where
  hClose     = H . S.hClose
  hPutStr h  = H . S.hPutStr h
  openFile h = H . S.openFile h
