{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module LogIO
  ( LogIO
  , LogIOAction(..)
  , LogIOHandle(LogIOHandle)
  , execLogIO
  ) where

import Control.Monad.Catch        ( MonadThrow
                                  , SomeException
                                  , throwM
                                  , toException )
import Control.Monad.Writer       ( WriterT(WriterT)
                                  , execWriterT )
import Control.Monad.Writer.Class ( MonadWriter(..) )
import MonadHandle
import System.IO                  ( IOMode(ReadMode) )
import System.IO.Error            ( illegalOperationErrorType
                                  , mkIOError )

newtype LogIO a = L (WriterT [LogIOAction] (Either SomeException) a)
  deriving (Functor, Applicative, Monad, MonadWriter [LogIOAction])

data LogIOHandle = LogIOHandle {
  path :: FilePath,
  mode :: IOMode
  } deriving (Eq, Show)

data LogIOAction
  = Open  FilePath    IOMode
  | Put   LogIOHandle String
  | Close LogIOHandle
  deriving (Eq, Show)

logIO :: x -> LogIOAction -> LogIO x
logIO x a = tell [a] >> return x

execLogIO (L w) = execWriterT w

instance MonadThrow LogIO where
  throwM = L . WriterT . Left . toException

instance MonadHandle LogIOHandle LogIO where
  hClose   h = logIO () $ Close h
  hPutStr  h str
    | mode h /= ReadMode = logIO () $ Put h str
    | otherwise = throwM
                  $ mkIOError illegalOperationErrorType p Nothing (Just p)
                  where p = path h
  openFile f mode = logIO (LogIOHandle f mode) $ Open f mode
