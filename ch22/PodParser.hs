{-# LANGUAGE OverloadedStrings #-}

module PodParser (parseEpisodes) where

import           Data.Default    (def)
import qualified Data.Text       as T
import qualified Data.Text.Lazy  as TL
import           PodTypes
import           Text.XML        (parseText_)
import           Text.XML.Cursor (attribute, element, fromDocument, ($//),
                                  (>=>))

data Feed = Feed { urls :: [T.Text] }

parseFeed :: TL.Text -> Feed
parseFeed = feed . parseText_ def

feed doc = Feed $ fromDocument doc $// element "enclosure" >=> attribute "url"

episodes p = map urlToEp . urls
  where urlToEp url = Episode 0 (T.unpack url) False p

parseEpisodes p = episodes p . parseFeed
