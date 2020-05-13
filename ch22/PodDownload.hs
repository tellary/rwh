{-# LANGUAGE OverloadedStrings #-}

module PodDownload where

import           Control.Concurrent.Async   (forConcurrently_)
import           Control.Lens               ((^.))
import qualified Data.ByteString.Char8      as S8 (pack, putStrLn)
import qualified Data.ByteString.Lazy.Char8 as SL8 (unpack, writeFile)
import qualified Data.Text.Lazy             as TL (pack)
import           Network.Wreq               (get, responseBody)
import qualified Network.Wreq.Session       as Sess (get, newSession)
import           PodDB                      (addEpisodeMaybe, updateEpisode)
import           PodParser                  (parseEpisodes)
import           PodTypes                   (Episode (epCast, epDone, epId, epUrl),
                                             Podcast (castId, castUrl))
import           Refined                    (unrefine)
import           Text.Printf                (printf)

downloadPodcastInfo conn p = do
  putStrLn
    $ printf "Downloading podcast info for %i from %s"
      (unrefine . castId  $ p)
      (unrefine . castUrl $ p)
  r <- get . unrefine . castUrl $ p
  let es = parseEpisodes p . TL.pack . SL8.unpack $ r ^. responseBody
  mapM_ (addEpisodeMaybe conn) es

downloadEpisode conn sess e = do
  S8.putStrLn . S8.pack
    $ printf "Downloading episode %i info for podcast %i from %s"
      (unrefine . epId $ e)
      (unrefine . castId . epCast $ e)
      (unrefine . epUrl $ e)
  r <- Sess.get sess . unrefine . epUrl $ e
  SL8.writeFile file $ r ^. responseBody
  updateEpisode conn e { epDone = True }
  where file      = castIdStr ++ "." ++ epIdStr ++ ".mp3"
        castIdStr = show . unrefine . castId . epCast $ e
        epIdStr   = show . unrefine . epId $ e

downloadEpisodes conn es = do
  sess <- Sess.newSession
  forConcurrently_ es $ downloadEpisode conn sess
