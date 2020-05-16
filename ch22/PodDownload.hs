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
import           PodTypes                   (Episode (epCast, epDone, epId, epUrl),
                                             Podcast (castId, castUrl))
import           Refined                    (unrefine)
import           Text.Printf                (printf)
import           UnliftIO.Async             (pooledForConcurrentlyN_)

numThreads = 10

addAndDownloadPodcast pool p0 = do
  sess <- Sess.newSession
  withResource pool $ \conn -> withTransaction conn $ do
    p <- addPodcast conn p0
    downloadPodcast0 conn sess p
    return p

downloadPodcast0 conn sess p = do
  S8.putStrLn . S8.pack
    $ printf "Downloading podcast info for %i from %s"
      (unrefine . castId  $ p)
      (unrefine . castUrl $ p)
  r <- Sess.get sess . unrefine . castUrl $ p
  let es = parseEpisodes p . TL.pack . SL8.unpack $ r ^. responseBody
  mapM_ (addEpisodeMaybe conn) es

downloadEpisode0 conn sess e = do
  S8.putStrLn . S8.pack
    $ printf "Downloading episode %i for podcast %i from %s"
      (unrefine . epId $ e)
      (unrefine . castId . epCast $ e)
      (unrefine . epUrl $ e)
  r <- Sess.get sess . unrefine . epUrl $ e
  SL8.writeFile file $ r ^. responseBody
  updateEpisode conn e { epDone = True }
  S8.putStrLn . S8.pack
    $ printf "Completed downloading episode %i for podcast %i from %s"
      (unrefine . epId $ e)
      (unrefine . castId . epCast $ e)
      (unrefine . epUrl $ e)
  where file      = castIdStr ++ "." ++ epIdStr ++ ".mp3"
        castIdStr = show . unrefine . castId . epCast $ e
        epIdStr   = show . unrefine . epId $ e

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
