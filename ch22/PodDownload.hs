{-# LANGUAGE OverloadedStrings #-}

module PodDownload
  ( addAndDownloadPodcast
  , downloadEpisodes
  , downloadPodcasts
  , numThreads
  ) where

import           Control.Lens               ((^.))
import qualified Data.ByteString.Char8      as S8 (pack, putStrLn)
import qualified Data.ByteString.Lazy.Char8 as SL8 (unpack, writeFile)
import           Data.Pool                  (withResource)
import qualified Data.Text.Lazy             as TL (pack)
import           Database.SQLite.Simple     (withTransaction)
import           Network.Wreq               (responseBody)
import qualified Network.Wreq.Session       as Sess (get, newSession)
import           PodDB                      (addPodcast)
import           PodDB                      (addEpisodeMaybe, updateEpisode)
import           PodParser                  (parseEpisodes)
import           PodTypes                   (Episode (epCast, epDone),
                                             unCastId, unCastUrl, unEpId,
                                             unEpUrl)
import           Text.Printf                (printf)
import           UnliftIO.Async             (pooledForConcurrentlyN_)

numThreads = 3

addAndDownloadPodcast pool p0 = do
  sess <- Sess.newSession
  withResource pool $ \conn -> withTransaction conn $ do
    p <- addPodcast conn p0
    downloadPodcast0 conn sess p
    return p

downloadPodcast0 conn sess p = do
  S8.putStrLn . S8.pack
    $ printf "Downloading podcast info for %i from %s"
      (unCastId  $ p) (unCastUrl $ p)
  r <- Sess.get sess . unCastUrl $ p
  let es = parseEpisodes p . TL.pack . SL8.unpack $ r ^. responseBody
  mapM_ (addEpisodeMaybe conn) es

downloadEpisode0 conn sess e = do
  S8.putStrLn . S8.pack
    $ printf "Downloading episode %i for podcast %i from %s"
      (unEpId $ e) (unCastId . epCast $ e) (unEpUrl $ e)
  r <- Sess.get sess . unEpUrl $ e
  SL8.writeFile file $ r ^. responseBody
  updateEpisode conn e { epDone = True }
  S8.putStrLn . S8.pack
    $ printf "Completed downloading episode %i for podcast %i from %s"
      (unEpId $ e) (unCastId . epCast $ e) (unEpUrl $ e)
  where file      = castIdStr ++ "." ++ epIdStr ++ ".mp3"
        castIdStr = show . unCastId . epCast $ e
        epIdStr   = show . unEpId $ e

downloadEpisodes pool es = do
  sess <- Sess.newSession
  pooledForConcurrentlyN_ numThreads es $ \e ->
    -- It's expensive to download an episode.
    -- An individual episode must be marked as done once downloaded.
    -- Running this in transaction to prevent downloading an episode
    -- as part of a larger transaction.
    withResource pool $ \conn ->
    withTransaction conn $ downloadEpisode0 conn sess e

downloadPodcasts pool ps = do
  sess <- Sess.newSession
  pooledForConcurrentlyN_ numThreads ps $ \p ->
    -- If a single episode of a podcast is added, hence all should be added,
    -- thus it's in transaction
    withResource pool $ \ conn ->
    withTransaction conn $ downloadPodcast0 conn sess p
