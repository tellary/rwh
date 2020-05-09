{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}
module PodTypes
  ( Episode(..)
  , Podcast(..)
  , PodId
  , PodUrl
  , episode
  , podcast
  ) where

import Language.Haskell.TH        (Q, TExp)
import Language.Haskell.TH.Syntax (Lift (..), TExp (TExp))
import Refined                    (NonEmpty, Positive, Refined, refineFail)

type PodId  = Refined Positive Int
type PodUrl = Refined NonEmpty String

liftTyped :: Lift a => a -> Q (TExp a)
liftTyped = fmap TExp . lift

data Podcast = Podcast
  { castId  :: PodId
  , castUrl :: PodUrl
  } deriving (Eq, Show, Lift)

data Episode = Episode
  { epId   :: PodId
  , epUrl  :: PodUrl
  , epDone :: Bool
  , epCast :: Podcast
  } deriving (Eq, Show, Lift)

podcast :: Int -> String -> Q (TExp Podcast)
podcast castId castUrl
  = liftTyped =<< Podcast <$> refineFail castId <*> refineFail castUrl

episode epId epUrl epDone epCast
  = liftTyped
  =<< Episode
  <$> refineFail epId
  <*> refineFail epUrl
  <*> pure epDone
  <*> pure epCast
