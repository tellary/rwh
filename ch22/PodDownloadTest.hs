{-# LANGUAGE TemplateHaskell #-}

import Control.Exception (catch, throw)
import Data.Pool         (withResource)
import PodDB             (initDB, listPodcastEpisodesByDone, mkPool)
import PodDownload       (addAndDownloadPodcast, downloadEpisodes, numThreads)
import PodTypes          (podcast)
import System.Directory  (removeFile)
import System.IO.Error   (doesNotExistErrorType, ioeGetErrorType)

d = do
  removeFile "pod-test.db"
    `catch` \e ->
              if doesNotExistErrorType == ioeGetErrorType e
              then return ()
              else throw e
  pool <- mkPool "pod-test.db" numThreads
  withResource pool initDB
  p <- addAndDownloadPodcast pool
    $$(podcast 666 "http://feed.thisamericanlife.org/talpodcast")
  es <- withResource pool $ \conn -> listPodcastEpisodesByDone conn False p
  downloadEpisodes pool es
  
main = d
