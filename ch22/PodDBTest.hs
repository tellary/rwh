{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

import Control.Exception      (catch, finally, throw)
import Data.IORef             (IORef, newIORef, readIORef, writeIORef)
import Database.SQLite.Simple (Connection, SQLError, changes, close)
import PodDB                  (PodDBError (PodcastAlreadyExists),
                               addEpisodeMaybe, addPodcast, deletePodcast,
                               deletePodcastEpisodes, getPodcast, initDB,
                               listPodcastEpisodesByDone, listPodcasts, openFK,
                               updateEpisode, updatePodcast)
import PodTypes               (episode, podcast)
import Refined                (refineTH)
import System.Directory       (removeFile)
import System.IO.Error        (doesNotExistErrorType, ioeGetErrorType)
import Test.HUnitPlus         (assertThrows, assertThrowsExact)
import Test.Tasty             (DependencyType (AllFinish, AllSucceed), TestTree,
                               after, defaultMain, testGroup)
import Test.Tasty.HUnit       (testCase, (@?=))

main = do
  connRef <- newIORef
             $ error "Connection must be init by the \"Init DB\" test"
  (defaultMain $ intTests testDbFile connRef)
    `finally` (readIORef connRef >>= close)

testDbFile = "pod-test.db"

intTests :: FilePath -> IORef Connection -> TestTree
intTests testDbFile connRef = testGroup "PodDB integration tests"
  [ testCase "Init DB" $ do
      removeFile testDbFile
        `catch` \e ->
                  if doesNotExistErrorType == ioeGetErrorType e
                  then return ()
                  else throw e
      conn <- openFK testDbFile
      initDB conn
      writeIORef connRef conn

  , after AllSucceed "Init DB"
    $ testCase "Add podcast" $ do
      conn <- readIORef connRef
      p    <- addPodcast conn $$(podcast 666 "castUrlOne")
      p @?= $$(podcast 1 "castUrlOne")

  , after AllSucceed "Add podcast"
    $ testCase "Duplicate podcast fails" $ do
      conn <- readIORef connRef
      assertThrowsExact (PodcastAlreadyExists $$(refineTH "castUrlOne"))
        (addPodcast conn $$(podcast 666 "castUrlOne"))

  , after AllSucceed "Add podcast"
    $ testCase "List single podcast" $ do
      conn  <- readIORef connRef
      casts <- listPodcasts conn
      casts @?= [$$(podcast 1 "castUrlOne")]

  , after AllSucceed "Add podcast"
    $ testCase "Get podcast" $ do
      conn   <- readIORef connRef
      Just p <- getPodcast conn $$(refineTH 1)
      p @?= $$(podcast 1 "castUrlOne")

  , after AllSucceed "Add podcast"
    $ after AllFinish "List single podcast"
    $ after AllFinish "Get podcast"
    $ testCase "Update podcast" $ do
      conn <- readIORef connRef
      updatePodcast conn $$(podcast 1 "castUrlOne'")
      [p]  <- listPodcasts conn
      p @?= $$(podcast 1 "castUrlOne'")

  , after AllSucceed "Update podcast"
    $ testCase "Add more podcasts" $ do
      conn <- readIORef connRef
      p2   <- addPodcast conn $$(podcast 666 "castUrlTwo")
      p2 @?= $$(podcast 2 "castUrlTwo")
      p3   <- addPodcast conn $$(podcast 666 "castUrlThree")
      p3 @?= $$(podcast 3 "castUrlThree")
      p4   <- addPodcast conn $$(podcast 666 "castUrlFour")
      p4 @?= $$(podcast 4 "castUrlFour")
      p5   <- addPodcast conn $$(podcast 666 "castUrlFive")
      p5 @?= $$(podcast 5 "castUrlFive")

  , after AllSucceed "Add more podcasts"
    $ testCase "List multiple podcasts" $ do
      conn  <- readIORef connRef
      casts <- listPodcasts conn
      casts @?=
        [ $$(podcast 1 "castUrlOne'")
        , $$(podcast 2 "castUrlTwo")
        , $$(podcast 3 "castUrlThree")
        , $$(podcast 4 "castUrlFour")
        , $$(podcast 5 "castUrlFive")
        ]

  , after AllSucceed "Add podcast"
    $ testCase "Add episode" $ do
      conn <- readIORef connRef
      r    <- addEpisodeMaybe conn
              $$(episode 666 "epUrlOne" False $$(podcast 1 "bla"))
      r @?= True
      c    <- changes conn
      c @?= 1

  , after AllSucceed "Add episode"
    $ testCase "No modification on duplicate URL episode" $ do
      conn <- readIORef connRef
      r    <- addEpisodeMaybe conn
              $$(episode 666 "epUrlOne" False $$(podcast 1 "bla"))
      r @?= False
      c    <- changes conn
      c @?= 0

  , after AllSucceed "Add episode" $
    testCase "List single podcast episode not done" $ do
      conn <- readIORef connRef
      [e]  <- listPodcastEpisodesByDone conn False $$(podcast 1 "foo")
      e @?= $$(episode 1 "epUrlOne" False $$(podcast 1 "foo"))

  , after AllSucceed "List single podcast episode not done" $
    testCase "Update episode" $ do
      conn <- readIORef connRef
      updateEpisode conn $$(episode 1 "epUrlOne'" True $$(podcast 1 "bla"))
      [e]  <- listPodcastEpisodesByDone conn True $$(podcast 1 "foo")
      e @?= $$(episode 1 "epUrlOne'" True $$(podcast 1 "foo"))

  , after AllSucceed "Init DB" $
    testCase "Add episode with nonexistent podcast fails" $ do
      conn <- readIORef connRef
      assertThrows (\(_ :: SQLError) -> return ()) $ do
        addEpisodeMaybe conn
          $$(episode 1 "nonCreatedUrl" True $$(podcast 666 "bla"))
        return ()
  , after AllFinish  "Update episode" $
    after AllSucceed "Add episode" $
    testCase "Delete podcast episodes" $ do
      conn <- readIORef connRef
      let p = $$(podcast 1 "irrelevant")
      deletePodcastEpisodes conn p
      es   <- (++) <$> listPodcastEpisodesByDone conn True  p
                   <*> listPodcastEpisodesByDone conn False p
      es @?= []
  , after AllFinish  "Update episode" $
    after AllFinish  "List multiple podcasts" $
    after AllSucceed "Delete podcast episodes" $
    testCase "Delete podcast" $ do
      conn <- readIORef connRef
      let p = $$(podcast 1 "irrelevant")
      deletePodcast conn p
      casts <- listPodcasts conn
      casts @?=
        [ $$(podcast 2 "castUrlTwo")
        , $$(podcast 3 "castUrlThree")
        , $$(podcast 4 "castUrlFour")
        , $$(podcast 5 "castUrlFive")
        ]
  ]
