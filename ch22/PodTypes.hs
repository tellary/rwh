module PodTypes where

data Podcast = Podcast
  { castId  :: Int
  , castUrl :: String
  } deriving (Eq, Show)

data Episode = Episode
  { epId   :: Int
  , epUrl  :: String
  , epDone :: Bool
  , epCast :: Podcast
  } deriving (Eq, Show)
