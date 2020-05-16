{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}
module PodTypes
  ( Episode(..)
  , Podcast(..)
  , PodId
  , PodUrl
  , episode
  , podcast
  , unCastId
  , unCastUrl
  , unEpId
  , unEpUrl
  ) where

import Language.Haskell.TH        (Q, TExp)
import Language.Haskell.TH.Syntax (Lift (..))
import Refined                    (NonEmpty, Positive, Refined, refineTH,
                                   unrefine)

type PodId  = Refined Positive Int
type PodUrl = Refined NonEmpty String

data Podcast = Podcast
  { castId  :: PodId
  , castUrl :: PodUrl
  } deriving (Eq, Show, Lift)

unCastId  = unrefine . castId
unCastUrl = unrefine . castUrl

data Episode = Episode
  { epId   :: PodId
  , epUrl  :: PodUrl
  , epDone :: Bool
  , epCast :: Podcast
  } deriving (Eq, Show, Lift)

unEpId  = unrefine . epId
unEpUrl = unrefine . epUrl

podcast :: Int -> String -> Q (TExp Podcast)
podcast castId castUrl
  = [|| Podcast $$(refineTH castId) $$(refineTH castUrl) ||]

episode epId epUrl epDone epCast
  = [|| Episode $$(refineTH epId) $$(refineTH epUrl) epDone epCast ||]
