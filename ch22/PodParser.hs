{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module PodParser (parseEpisodes) where

import           Data.Default               (def)
import qualified Data.Text                  as T (Text, unpack)
import qualified Data.Text.Lazy             as TL (Text)
import           Language.Haskell.TH.Syntax (Exp (ListE), Q, TExp (..))
import           PodTypes                   (Episode (Episode), Podcast,
                                             episode)
import           Refined                    (refine, refineTH)
import           Text.XML                   (parseText_)
import           Text.XML.Cursor            (attribute, element, fromDocument,
                                             ($//), (>=>))

data Feed = Feed { urls :: [T.Text] } deriving (Eq, Show)

parseFeed :: TL.Text -> Feed
parseFeed = feed . parseText_ def

feed doc = Feed $ fromDocument doc $// element "enclosure" >=> attribute "url"

episodesTH :: Podcast -> Feed -> Q (TExp [Episode])
episodesTH p = fmap TExp . fmap ListE . sequence . map urlToEp . urls
  where urlToEp url = unType <$> episode 666 (T.unpack url) False p

episodes p = map urlToEp . urls
  where urlToEp url
          = Episode $$(refineTH 666) (refineUrl $ T.unpack url) False p
        refineUrl url
          = either
            (const $ error "Empty url not allowed")
            id
          $ refine url

parseEpisodes p = episodes p . parseFeed
