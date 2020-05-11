{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}
module PodTypes
  ( Episode(..)
  , Podcast(..)
  , PodId
  , PodUrl
  , episode
  , episodeThrow
  , liftTyped
  , podcast
  ) where

import Language.Haskell.TH        (Q)
import Language.Haskell.TH.Syntax (Lift (..), TExp (TExp))
import Refined                    (NonEmpty, Positive, Refined, refineTH,
                                   refineThrow)

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
  = [|| Podcast $$(refineTH castId) $$(refineTH castUrl) ||]

episodeThrow epId epUrl epDone epCast
  = Episode
  <$> refineThrow epId
  <*> refineThrow epUrl
  <*> pure epDone
  <*> pure epCast

episode epId epUrl epDone epCast
  = liftTyped =<< episodeThrow epId epUrl epDone epCast
