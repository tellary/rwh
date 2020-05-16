{-# LANGUAGE TemplateHaskell #-}

import qualified Data.Text.Lazy as TL (pack)
import           PodParser      (parseEpisodes)
import           PodTypes       (Episode (epUrl), podcast)
import           Refined        (unrefine)
import           Test.Hspec     (describe, hspec, it, shouldReturn)

urls f
  = map (unrefine . epUrl)
  . parseEpisodes $$(podcast 1 "ignore")
  . TL.pack <$> readFile f

main = hspec $ do
  describe "parseEpisodes" $ do
    it "should parse `example-rss.xml`" $
      urls "data/example-rss.xml"
      `shouldReturn`
      [ "http://www.example.com/radio/lambdas.mp3"
      , "http://www.example.com/radio/parsec.mp3"
      ]

    it "should parse `thisamericanlife.org-rss.xml`" $
       urls "data/thisamericanlife.org-rss.xml"
      `shouldReturn`
      [ "https://www.podtrac.com/pts/redirect.mp3/dovetail.prxu.org/188/\
        \8f02bc01-6966-44d0-9dcc-a827aadd51db/186.mp3"
      , "https://www.podtrac.com/pts/redirect.mp3/dovetail.prxu.org/188/\
        \32b9878d-7da9-42af-bf6a-8deea635f846/702.mp3"
      , "https://www.podtrac.com/pts/redirect.mp3/dovetail.prxu.org/188/\
        \4c9e3651-7228-48af-900c-38fa331a4466/701.mp3"
      , "https://www.podtrac.com/pts/redirect.mp3/dovetail.prxu.org/188/\
        \e1b844c9-b14c-4e8f-b885-260bf3775f7d/700.mp3"
      , "https://www.podtrac.com/pts/redirect.mp3/dovetail.prxu.org/188/\
        \7e31e3bc-fdde-4faa-ab55-ba4d7dc54d6f/699.mp3"
      , "https://www.podtrac.com/pts/redirect.mp3/dovetail.prxu.org/188/\
        \5053513b-a8ca-40bc-b80a-3ee74a132cfc/698.mp3"
      , "https://www.podtrac.com/pts/redirect.mp3/dovetail.prxu.org/188/\
        \091d67bc-c517-4ea4-91db-ed5b34f9b8ef/697.mp3"
      , "https://www.podtrac.com/pts/redirect.mp3/dovetail.prxu.org/188/\
        \85067b18-2a2c-410f-bb7d-2c0c54c2571c/696.mp3"
      , "https://www.podtrac.com/pts/redirect.mp3/dovetail.prxu.org/188/\
        \1b2d066f-e3b7-4d4d-b20d-2ed2248aeccc/695.mp3"
      , "https://www.podtrac.com/pts/redirect.mp3/dovetail.prxu.org/188/\
        \d77e669d-5ec0-4d41-a315-6baeae56e9db/694.mp3"
      ]
