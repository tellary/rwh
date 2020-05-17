{-# LANGUAGE TemplateHaskell #-}

import Control.Exception      (Exception (displayException), SomeException,
                               handle)
import Control.Monad          (forM, forM_)
import Data.List              (isPrefixOf, isSuffixOf, transpose)
import Data.Pool              (destroyAllResources, withResource)
import Data.Semigroup         ((<>))
import Database.SQLite.Simple (withTransaction)
import Options.Applicative    (argument, command, idm, info, metavar, progDesc,
                               str)
import OptParseREPL           (repl)
import PodDB                  (deletePodcast, deletePodcastEpisodes, getPodcast,
                               initDB, listPodcastEpisodesByDone, listPodcasts,
                               mkPool)
import PodDownload            (addAndDownloadPodcast, downloadEpisodes,
                               downloadPodcasts, numThreads)
import PodTypes               (Podcast (Podcast), unCastId, unCastUrl)
import Refined                (refineFail, refineTH)
import System.Directory       (listDirectory, removeFile)
import Text.Printf            (printf)

data Command = Add String
             | List
             | Update
             | Download
             | Delete String
             | Quit
             deriving Show

cmds
  =  command "add"
     (info
       (Add <$> argument str (metavar "URL"))
       (progDesc "add new podcast URL")
     )
  <> command "list"
     (info
       (pure List)
       (progDesc "list podcasts")
     )
  <> command "update"
     (info
       (pure Update)
       (progDesc "retrieve updates from podcast URLs")
     )
  <> command "download"
     (info
       (pure Download)
       (progDesc "download episodes for all podcasts")
     )
  <> command "delete"
     (info
       (Delete <$> argument str (metavar "ID"))
       (progDesc "delete podcast and its episodes by ID")
     )
  <> command "quit"
     (info
       (pure Quit )
       idm
     )

main = do
  pool <- mkPool "pod.db" numThreads
  withResource pool initDB
  replLoop pool
  destroyAllResources pool

replLoop pool = do
  cmd <- repl cmds
  handle (\e -> do
             putStrLn $ displayException (e :: SomeException)
             replLoop pool)
    $ case cmd of
        Add url   -> do
          addAndDownloadPodcast pool
            =<< Podcast $$(refineTH 666)
            <$> refineFail url
          replLoop pool
        List      -> do
          ps <- withResource pool listPodcasts
          forM_ ps $ \p -> putStrLn $ printf "%i %s" (unCastId p) (unCastUrl p)
          replLoop pool
        Update    -> do
          ps <- withResource pool listPodcasts
          downloadPodcasts pool ps
          replLoop pool
        Download  -> do
          es <- withResource pool $ \conn -> do
            ps <- listPodcasts conn
            fmap (concat . transpose) . forM ps
              $ listPodcastEpisodesByDone conn False
          downloadEpisodes pool es
          replLoop pool
        Delete id -> do
          -- We delete files after database changes are done, so that an
          -- exception while deleting files rolls the database changes back.
          withResource pool $ \conn -> withTransaction conn $ do
            Just p <- getPodcast conn =<< (refineFail . read $ id)
            deletePodcastEpisodes conn p
            deletePodcast         conn p
            mapM_ removeFile
              =<< filter (".mp3"      `isSuffixOf`)
              .   filter ((id ++ ".") `isPrefixOf`)
              <$> listDirectory "."
          replLoop pool
        Quit      -> return ()
