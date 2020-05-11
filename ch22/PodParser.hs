{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module PodParser (parseEpisodes) where

import           Control.Monad.Catch        (MonadThrow)
import           Data.Default               (def)
import qualified Data.Text                  as T (Text, unpack)
import qualified Data.Text.Lazy             as TL (Text)
import           Language.Haskell.TH.Syntax (Q, TExp (..))
import           PodTypes                   (Episode, Podcast, episodeThrow,
                                             liftTyped)
import           Text.XML                   (parseText_)
import           Text.XML.Cursor            (attribute, element, fromDocument,
                                             ($//), (>=>))

data Feed = Feed { urls :: [T.Text] } deriving (Eq, Show)

parseFeed :: TL.Text -> Feed
parseFeed = feed . parseText_ def

feed doc = Feed $ fromDocument doc $// element "enclosure" >=> attribute "url"

episodes :: Podcast -> Feed -> Q (TExp [Episode])
episodes p f = liftTyped =<< (episodesThrow p $ f)

episodesThrow :: MonadThrow m => Podcast -> Feed -> m [Episode]
episodesThrow p = mapM urlToEp . urls
  where urlToEp url = episodeThrow 666 (T.unpack url) False p

parseEpisodes p = either (error . show) id . episodesThrow p . parseFeed
