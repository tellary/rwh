{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module PodParser (parseEpisodes) where

import           Data.Default    (def)
import qualified Data.Text       as T (unpack)
import qualified Data.Text.Lazy  as TL (Text)
import           PodTypes        (Episode (..), PodUrl, Podcast)
import           Refined         (refineTH, refineThrow)
import           Text.XML        (Document, parseText_)
import           Text.XML.Cursor (attribute, element, fromDocument, ($//),
                                  (>=>))
data Feed = Feed { urls :: [PodUrl] } deriving (Eq, Show)

feed :: Document -> Feed
feed doc
  = either (\e -> error $ "Empty url is found while parsing: " ++ show e) id
  . fmap Feed . mapM (refineThrow . T.unpack)
  $ fromDocument doc $// element "enclosure" >=> attribute "url"

parseFeed :: TL.Text -> Feed
parseFeed = feed . parseText_ def

episodes :: Podcast -> Feed -> [Episode]
episodes p = map urlToEp . urls
  where urlToEp url = Episode $$(refineTH 666) url False p

parseEpisodes :: Podcast -> TL.Text -> [Episode]
parseEpisodes p = episodes p . parseFeed
