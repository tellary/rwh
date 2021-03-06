{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CountFoldersContent
  ( App
  , AppConfig (..)
  , AppState  (..)
  , runApp
  , runCountFoldersContent
  ) where

import           Control.Monad          (filterM, when)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Control.Monad.Reader   (MonadReader (ask), ReaderT, runReaderT)
import           Control.Monad.State    (MonadState, StateT, get, put,
                                         execStateT)
import           Control.Monad.Writer   (MonadWriter (tell), WriterT,
                                         runWriterT)
import qualified Data.Map               as M
import           Data.Tuple (swap)
import           System.Directory       (doesDirectoryExist, listDirectory)

data AppConfig
  = AppConfig { cfgMaxDepth :: Int }
  deriving Show

data AppState
  = AppState  { stMaxDepth  :: Int }
  deriving (Eq, Show)

newtype App a = A {
  unApp ::
      StateT AppState
      (
        WriterT
        ( M.Map FilePath Int   )
        ( ReaderT AppConfig IO )
      )
      a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader AppConfig
    , MonadState  AppState
    , MonadWriter (M.Map FilePath Int)
    )

countFoldersContent0 :: Int -> FilePath -> App ()
countFoldersContent0 depth path = do
  maxDepth <- cfgMaxDepth <$> ask
  when (depth <= maxDepth) $ do
    visitedMaxDepth <- stMaxDepth  <$> get
    when (visitedMaxDepth < depth) $ put (AppState depth)
    contents        <- liftIO . listDirectory $ path
    tell . M.singleton path . length $ contents
    dirs            <- liftIO
                       . filterM doesDirectoryExist
                       . map addPrefix
                       $ contents
    mapM_ (countFoldersContent0 $ depth + 1) dirs
  where addPrefix file = path ++ "/" ++ file

countFoldersContent = countFoldersContent0 0

runApp cfg st = fmap swap . (`runReaderT` cfg) . runWriterT . (`execStateT` st) . unApp

runCountFoldersContent maxDepth path
  = runApp (AppConfig maxDepth) (AppState 0) (countFoldersContent path)


