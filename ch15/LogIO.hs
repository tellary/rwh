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

import           Control.Monad.Catch        (MonadCatch, MonadMask,
                                             MonadThrow (..), SomeException)
import           Control.Monad.State        (MonadState, StateT, evalStateT,
                                             gets, modify)
import           Control.Monad.Writer       (WriterT (WriterT), execWriterT,
                                             runWriterT)
import           Control.Monad.Writer.Class (MonadWriter (..))
import           Data.DList                 (DList, singleton, toList)
import qualified Data.Map                   as M
import           MonadHandle
import           System.IO                  (IOMode (ReadMode))
import           System.IO.Error            (illegalOperationErrorType,
                                             ioeSetErrorString, mkIOError)

newtype LogIO a = L
  (WriterT
    (DList LogIOAction)
    (StateT
     (M.Map FilePath LogIOHandle)
     (Either SomeException)
    ) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadWriter (DList LogIOAction)
    , MonadState (M.Map FilePath LogIOHandle)
    , MonadThrow
    , MonadCatch
    , MonadMask )

data LogIOHandle = LogIOHandle {
  mode   :: IOMode
  } deriving (Eq, Show)

data LogIOAction
  = Open  FilePath IOMode
  | Put   FilePath String
  | Close FilePath
  deriving (Eq, Show)

logIO :: LogIOAction -> LogIO ()
logIO a = tell (singleton a)

execLogIO :: LogIO a -> Either SomeException [LogIOAction]
execLogIO (L w) = fmap toList . (`evalStateT` M.empty) . execWriterT $ w
evalLogIO :: LogIO a -> Either SomeException a
evalLogIO (L w) = fmap fst . (`evalStateT` M.empty) . runWriterT $ w

instance MonadHandle FilePath LogIO where
  hClose   f = do
    modify $ M.delete f
    logIO $ Close f
    return ()
  hPutStr  f str = do
    let n = "hPutStr"
        e = mkIOError illegalOperationErrorType n Nothing (Just f)
    hm <- gets $ M.lookup f
    case hm of
      Nothing -> throwM $ ioeSetErrorString e "handle is closed"
      Just h
        | mode h == ReadMode
          -> throwM $ ioeSetErrorString e "handle is in read mode"
        | otherwise
          -> (logIO $ Put f str) >> return ()
  openFile f mode = do
    logIO $ Open f mode
    modify $ M.insert f $ LogIOHandle mode
    return f

