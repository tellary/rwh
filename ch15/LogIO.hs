{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module LogIO (LogIO, LogIOAction(..), execLogIO) where

import Control.Monad.Writer (Writer, execWriter)
import Control.Monad.Writer.Class (MonadWriter(..))
import MonadHandle
import System.IO (Handle, IOMode)

newtype LogIO a = L (Writer [LogIOAction] a)
  deriving (Functor, Applicative, Monad, MonadWriter [LogIOAction])

data LogIOAction
  = Open FilePath IOMode
  | Put FilePath String
  | Close FilePath
  deriving (Show, Eq)

logIO :: x -> LogIOAction -> LogIO x
logIO x a = tell [a] >> return x

execLogIO (L w) = execWriter w

instance MonadHandle FilePath LogIO where
  hClose   h      = logIO () $ Close h
  hPutStr  h str  = logIO () $ Put   h str
  openFile f mode = logIO f  $ Open  f mode

