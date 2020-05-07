import Control.Exception      (catch, finally, throw)
import Data.IORef             (IORef, newIORef, readIORef, writeIORef)
import Database.SQLite.Simple (Connection, changes, close, open)
import PodDB
import PodTypes
import System.Directory       (removeFile)
import System.IO.Error        (doesNotExistErrorType, ioeGetErrorType)
import Test.HUnitPlus         (assertThrowsExact)
import Test.Tasty             (DependencyType (AllSucceed), TestTree, after,
                               defaultMain, testGroup)
import Test.Tasty.HUnit       ((@?=), testCase)

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
      conn <- open testDbFile
      initDB conn
      writeIORef connRef conn

  , after AllSucceed "Init DB"
    $ testCase "Add podcast" $ do
      conn <- readIORef connRef
      p    <- addPodcast conn $ Podcast 0 "castUrlOne"
      p @?= Podcast 1 "castUrlOne"

  , after AllSucceed "Add podcast"
    $ testCase "Duplicate podcast fails" $ do
      conn <- readIORef connRef
      assertThrowsExact (PodcastAlreadyExists 1)
        (addPodcast conn $ Podcast 0 "castUrlOne")

  , after AllSucceed "Add podcast"
    $ testCase "List single podcast" $ do
      conn  <- readIORef connRef
      casts <- listPodcasts conn
      casts @?= [Podcast 1 "castUrlOne"]

  , after AllSucceed "List single podcast"
    $ testCase "Update podcast" $ do
      conn <- readIORef connRef
      updatePodcast conn $ Podcast 1 "castUrlOne'"
      [p]  <- listPodcasts conn
      p @?= Podcast 1 "castUrlOne'"

  , after AllSucceed "Update podcast"
    $ testCase "Add more podcasts" $ do
      conn <- readIORef connRef
      p2   <- addPodcast conn $ Podcast 0 "castUrlTwo"
      p2 @?= Podcast 2 "castUrlTwo"
      p3   <- addPodcast conn $ Podcast 0 "castUrlThree"
      p3 @?= Podcast 3 "castUrlThree"
      p4   <- addPodcast conn $ Podcast 0 "castUrlFour"
      p4 @?= Podcast 4 "castUrlFour"
      p5   <- addPodcast conn $ Podcast 0 "castUrlFive"
      p5 @?= Podcast 5 "castUrlFive"

  , after AllSucceed "Add more podcasts"
    $ testCase "List multiple podcasts" $ do
      conn  <- readIORef connRef
      casts <- listPodcasts conn
      casts @?=
        [ Podcast 1 "castUrlOne'"
        , Podcast 2 "castUrlTwo"
        , Podcast 3 "castUrlThree"
        , Podcast 4 "castUrlFour"
        , Podcast 5 "castUrlFive"
        ]

  , after AllSucceed "Add podcast"
    $ testCase "Add episode" $ do
      conn <- readIORef connRef
      r    <- addEpisodeMaybe conn
              $ Episode 0 "epUrlOne" False $ Podcast 1 "bla"
      r @?= True
      c    <- changes conn
      c @?= 1

  , after AllSucceed "Add episode"
    $ testCase "No modification on duplicate URL episode" $ do
      conn <- readIORef connRef
      r    <- addEpisodeMaybe conn
              $ Episode 0 "epUrlOne" False $ Podcast 1 "bla"
      r @?= False
      c    <- changes conn
      c @?= 0

  , after AllSucceed "Add episode" $
    testCase "List single podcast episode not done" $ do
      conn <- readIORef connRef
      [e]  <- listPodcastEpisodesByDone conn (Podcast 1 "foo") False
      e @?= Episode 1 "epUrlOne" False (Podcast 1 "foo")

  , after AllSucceed "List single podcast episode not done" $
    testCase "Update episode" $ do
      conn <- readIORef connRef
      updateEpisode conn $ Episode 1 "epUrlOne'" True (Podcast 1 undefined)
      [e]  <- listPodcastEpisodesByDone conn (Podcast 1 "foo") True
      e @?= Episode 1 "epUrlOne'" True (Podcast 1 "foo")
  ]
