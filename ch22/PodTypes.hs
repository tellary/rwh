{-# LANGUAGE DeriveLift            #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

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

import Control.Monad              (unless)
import Data.Maybe                 (fromJust, isJust)
import Data.String                (IsString (fromString))
import Data.Typeable              (typeOf)
import Language.Haskell.TH        (Q, TExp)
import Language.Haskell.TH.Syntax (Lift (..))
import Network.URI                (URI (uriAuthority, uriScheme),
                                   URIAuth (uriRegName), parseURI)
import Refined                    (Positive, Predicate (..), Refined, refineTH,
                                   throwRefineOtherException, unrefine)
import Text.Printf                (printf)

type PodId  = Refined Positive      Int
type PodUrl = Refined HttpUrlString String

data HttpUrlString = HttpUrlString

instance Predicate HttpUrlString String where
  validate p str = do
    u <- case parseURI str of
           Nothing -> throwRefineOtherException (typeOf p)
                      . fromString $ printf "Can't parse '%s' as URI" str
           Just u -> return u
    let s = uriScheme u
    unless (s `elem` ["http:", "https:"])
      . throwEx
      $ printf "'http:' or 'https:' scheme is expected but '%s' is found" s
    unless (isJust . uriAuthority $ u)
      . throwEx $ printf "Authority not found in '%s'" str
    unless (not . null . uriRegName . fromJust . uriAuthority $ u)
      . throwEx $ printf "Authority name not found in '%s'" str
    where throwEx = throwRefineOtherException (typeOf p) . fromString

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
