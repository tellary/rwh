{-# LANGUAGE TemplateHaskell #-}

import Control.Exception      (Exception (displayException), SomeException,
                               handle)
import Control.Monad          (forM)
import Data.Pool              (destroyAllResources, withResource)
import Data.Semigroup         ((<>))
import Options.Applicative    (argument, command, idm, info, metavar, progDesc,
                               str)
import OptParseREPL           (repl)
import PodDB                  (initDB, listPodcastEpisodesByDone, listPodcasts,
                               mkPool)
import PodDownload            (addAndDownloadPodcast, downloadEpisodes,
                               downloadPodcasts, numThreads)
import PodTypes               (Podcast (Podcast))
import Refined                (refine, refineTH)

data Command = Add String
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
       (Delete <$> argument str (metavar "URL"))
       (progDesc "delete podcast and its episodes")
     )
  <> command "quit"
     (info
       (pure Quit )
       idm
     )

podUrl url = either
             (const
              . error
              $ "Impossible to get an empty url from a command"
             ) id $ refine url

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
        -- Execute this command in transaction so that a podcast isn't added
        Add url -> do
          addAndDownloadPodcast pool $ Podcast $$(refineTH 666) (podUrl url)
          replLoop pool
        Update -> do
          ps <- withResource pool listPodcasts
          downloadPodcasts pool ps
          replLoop pool
        Download -> do
          es <- withResource pool $ \conn -> do
            ps <- listPodcasts conn
            fmap concat . forM ps $ listPodcastEpisodesByDone conn False
          downloadEpisodes pool es
        Quit -> return ()