{-# LANGUAGE OverloadedStrings #-}

module PodDB
  ( PodDBError(..)
  , addEpisodeMaybe
  , addPodcast
  , initDB
  , listPodcastEpisodesByDone
  , listPodcasts
  , updateEpisode
  , updatePodcast
  ) where

import Control.Exception                (Exception, catch, throw)
import Control.Monad                    (when)
import Data.Typeable                    (Typeable)
import Database.SQLite.Simple           (Connection, FromRow (..),
                                         ResultError (ConversionFailed),
                                         SQLError (sqlErrorDetails), changes,
                                         execute, execute_, field,
                                         lastInsertRowId, query, query_)
import Database.SQLite.Simple.FromField (FromField (..), returnError)
import Database.SQLite.Simple.ToField   (ToField (..))
import PodTypes                         (Episode (..), PodId, Podcast (..))
import Refined                          (Predicate, Refined, refine, refineFail,
                                         unrefine)

instance (Typeable a, Predicate p a, FromField a)
    => FromField (Refined p a) where
  fromField f = do
    v <- fromField f
    case refine v of
      Right r -> pure r
      Left  e -> returnError ConversionFailed f (show e)

instance (Predicate p a, ToField a) => ToField (Refined p a) where
  toField = toField . unrefine

sqlPodTables     = "SELECT name FROM sqlite_master \n\
                   \WHERE type='table' \n\
                   \AND name IN ('podcast', 'episode')"

sqlCreatePodcast = "CREATE TABLE podcast (\n\
                   \  id  INTEGER PRIMARY KEY AUTOINCREMENT,\n\
                   \  url VARCHAR(1024) NOT NULL UNIQUE)"

sqlCreateEpisode = "CREATE TABLE episode (\n\
                   \  id     INTEGER PRIMARY KEY AUTOINCREMENT,\n\
                   \  url    VARCHAR(1024) NOT NULL,\n\
                   \  done   INTEGER (1) NOT NULL DEFAULT 0,\n\
                   \  castId INTEGER NOT NULL,\n\
                   \  FOREIGN KEY (castId) REFERENCES podcast(id),\n\
                   \  UNIQUE (castId, url))"

sqlInsertPodcast = "INSERT INTO podcast(url) VALUES (?)"
sqlUpdatePodcast = "UPDATE podcast SET url = ? WHERE id = ?"
sqlListPodcasts  = "SELECT * FROM podcast"

sqlInsertEpisode = "INSERT INTO episode (url, done, castId) VALUES (?, ?, ?)"
sqlUpdateEpisode = "UPDATE episode\n\
                   \SET url = ?, done = ?, castId = ?\n\
                   \WHERE id = ?"

sqlListPodcastEpisodesByDone = "SELECT id, url, done FROM episode\n\
                               \WHERE castId = ? AND done = ?"

data PodDBError
  = PodcastAlreadyExists PodId
  | NoSuchEpisode PodId
  | NoSuchPodcast PodId
  deriving (Eq, Show)

instance Exception PodDBError

instance FromRow Podcast where
  fromRow = Podcast <$> field <*> field

initDB conn = do
  tables <- concat <$> query_ conn sqlPodTables :: IO [String]
  when ("podcast" `notElem` tables) $ execute_ conn sqlCreatePodcast
  when ("episode" `notElem` tables) $ execute_ conn sqlCreateEpisode

addPodcast conn p = do
  execute conn sqlInsertPodcast [castUrl p]
    `catch` \e ->
              if "UNIQUE constraint failed: podcast.url"
                 == sqlErrorDetails e
              then throw . PodcastAlreadyExists $ castId p
              else throw e
  id <- refineFail . fromIntegral =<< lastInsertRowId conn
  return $ p { castId = id }

updatePodcast conn p = do
  execute conn sqlUpdatePodcast (castUrl p, castId p)
  c <- changes conn
  case c of
    0 -> throw . NoSuchPodcast $ castId p
    1 -> return ()
    _ -> fail "Multiple podcast ids aren't possible"

listPodcasts :: Connection -> IO [Podcast]
listPodcasts conn = do
  query_ conn sqlListPodcasts

{- | Adds new episode to the database.
Quitely proceeds if episode with the same `(url, castId)` already exists.
The function is idempotent, so that it can be used to add episodes
from an RSS feed when the same episode can be added over and over again.
It can not return `Episode`, because we don't know episode id
in case of the unique constraint violation -}
addEpisodeMaybe :: Connection -> Episode -> IO Bool
addEpisodeMaybe conn e = do
    execute conn sqlInsertEpisode (epUrl e, epDone e, castId . epCast $ e)
    return True
  `catch` \e ->
    if "UNIQUE constraint failed: episode.castId, episode.url"
      == sqlErrorDetails e
    then return False
    else throw e

updateEpisode :: Connection -> Episode -> IO ()
updateEpisode conn e = do
  execute conn sqlUpdateEpisode (epUrl e, epDone e, castId . epCast $ e, epId e)
  c <- changes conn
  case c of
    0 -> throw . NoSuchEpisode $ epId e
    1 -> return ()
    _ -> fail "Multiple episode ids aren't possible"

listPodcastEpisodesByDone :: Connection -> Podcast -> Bool -> IO [Episode]
listPodcastEpisodesByDone conn p done = do
  rows <- query conn sqlListPodcastEpisodesByDone (castId $ p, done)
  return $ map ep rows
  where ep (id, url, done) = Episode id url done p
