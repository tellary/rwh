{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module LogIO
  ( LogIO
  , LogIOAction(..)
  , LogIOHandle(LogIOHandle)
  , execLogIO
  , evalLogIO
  ) where

import Control.Monad.Catch        ( MonadCatch
                                  , MonadMask
                                  , MonadThrow(..)
                                  , SomeException )
import Control.Monad.Writer       ( WriterT(WriterT)
                                  , execWriterT
                                  , runWriterT )
import Control.Monad.Writer.Class ( MonadWriter(..) )
import MonadHandle
import System.IO                  ( IOMode(ReadMode) )
import System.IO.Error            ( illegalOperationErrorType
                                  , mkIOError )

newtype LogIO a = L (WriterT [LogIOAction] (Either SomeException) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadWriter [LogIOAction]
    , MonadThrow
    , MonadCatch
    , MonadMask )

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

execLogIO :: LogIO a -> Either SomeException [LogIOAction]
execLogIO (L w) = execWriterT w
evalLogIO :: LogIO a -> Either SomeException a
evalLogIO (L w) = fmap fst . runWriterT $ w

instance MonadHandle LogIOHandle LogIO where
  hClose   h = logIO () $ Close h
  hPutStr  h str
    | mode h /= ReadMode = logIO () $ Put h str
    | otherwise = throwM
                  $ mkIOError illegalOperationErrorType p Nothing (Just p)
                  where p = path h
  openFile f mode = logIO (LogIOHandle f mode) $ Open f mode
