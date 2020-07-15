import           Codec.Picture   (convertRGB8, decodeImage)
import qualified Data.ByteString as B
import           Helper          (ean13, errorCutoff, validateURL)
import           Model           (EAN13 (..))
import           Test.Hspec      (describe, hspec, it, shouldBe, shouldReturn)

fileEan13 :: FilePath -> IO (Either String EAN13)
fileEan13 f = do
  bs <- B.readFile f
  let img = fmap convertRGB8 $ decodeImage bs
  return $ ean13 =<< img

goodFileEan13 :: FilePath -> IO (Maybe [Int])
goodFileEan13 f = do
  e <- fileEan13 f
  case e of
    Right e | eanError e < errorCutoff -> return . Just . eanDigits $ e
    _                                  -> return Nothing

goodFiles =
  [ ("ean13_1.jpg" , [1,2,3,4,5,6,7,8,9,0,1,2,8])
  , ("ean13_3.jpg" , [0,7,0,5,6,3,2,0,8,5,9,4,3])
  , ("ean13_4.jpg" , [3,8,0,0,0,6,5,7,1,1,1,3,5])
  , ("ean13_5.jpg" , [4,9,0,2,5,0,6,3,0,4,9,1,9])
  , ("ean13_6.jpeg", [0,0,7,2,5,1,2,0,4,4,9,0,2])
  , ("ean13_7.jpeg", [8,7,1,8,8,6,8,6,6,9,0,7,0])
  ]

imgUrl = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAARMAAAC3CAMAAAAGjUrGAAAAflBMVEX///8jHyAAAAAgGx0XERMSCw0GAAAtKSrd3d0NAwaioKGPjo4aFRbJyMgwLS5iYGBST0/39/ezsrLl5eUoJCW9vLyrqqpycHCJh4g8OTlNSkrY19ejoqLy8vLOzc2WlZZ5eHhZVldqaGl/fX1EQUFIRUY/Ozy5uLhdWltubGwA8V49AAAIlklEQVR4nO2YaZuiOhCFQ0GiARQBEVwQF1z6///BW1nAqD0OPXN7rs+dOh8as5Dl5aSSNGMkEolEIv2XyqImZdsoqk2yQUVrllbRRCWxYGxrNY19Y42/ow1jC3wzV/laUc7GEf6pVenWVm2iha6nVafqb1WremOshz0UkXmTYV6jK0/Us9APNQ7GTA+pKVioRjeq8yyq+ikUkW4CC7CeUq3fDu27OqvCRte23k/1AdOULQBsbV9yDgWrAXRbWKDZlMAl7ygCVpkxdgWejvG3EYzZBGDLxiA5LGzVABLG9rqOjPPUU43Xqt6E5QAhq1QRKIKFavTKWKSeFQt1QaG+Cv6SXqpzVAXGTnBSA4J+ChXA2hSMzJdVY+CiZWv1rq+zABvNAMaDmJR8mbJMdExGvudpJjLUTITQTHaB53sdE+F58Z6xQ+AhE8+KIxOhmASeJzomXnBgLIlVuT"

nonImgUrl = "data:someType;base64,iVBORw0KGgoAAAANSUhEUgAAARMAAAC3CAMAAAAGjUrGAAAAflBMVEX///8jHyAAAAAgGx0XERMSCw0GAAAtKSrd3d0NAwaioKGPjo4aFRbJyMgwLS5iYGBST0/39/ezsrLl5eUoJCW9vLyrqqpycHCJh4g8OTlNSkrY19ejoqLy8vLOzc2WlZZ5eHhZVldqaGl/fX1EQUFIRUY/Ozy5uLhdWltubGwA8V49AAAIlklEQVR4nO2YaZuiOhCFQ0GiARQBEVwQF1z6///BW1nAqD0OPXN7rs+dOh8as5Dl5aSSNGMkEolEIv2XyqImZdsoqk2yQUVrllbRRCWxYGxrNY19Y42/ow1jC3wzV/laUc7GEf6pVenWVm2iha6nVafqb1WremOshz0UkXmTYV6jK0/Us9APNQ7GTA+pKVioRjeq8yyq+ikUkW4CC7CeUq3fDu27OqvCRte23k/1AdOULQBsbV9yDgWrAXRbWKDZlMAl7ygCVpkxdgWejvG3EYzZBGDLxiA5LGzVABLG9rqOjPPUU43Xqt6E5QAhq1QRKIKFavTKWKSeFQt1QaG+Cv6SXqpzVAXGTnBSA4J+ChXA2hSMzJdVY+CiZWv1rq+zABvNAMaDmJR8mbJMdExGvudpJjLUTITQTHaB53sdE+F58Z6xQ+AhE8+KIxOhmASeJzomXnBgLIlVuT"

nonImgUrlShort = "data:someType;base64,blahblah"

main = hspec $ do
  describe "Helper" $ do
    it "should work on good files" $
      ( fmap sequence
      . mapM goodFileEan13
      . map ("samples/" ++)
      . fst
      . unzip $ goodFiles
      ) `shouldReturn`
      ( Just . snd . unzip $ goodFiles )

  describe "validateURL" $ do
    it "should fail to parse URI witout schema" $
      validateURL "aaa" `shouldBe` Just "Can't parse 'aaa' as URI"
    it "should check for HTTP scheme" $
      validateURL "ftp://blah.com" `shouldBe`
      Just "'http:', 'https:' or 'data:' schemes are expected, but 'ftp:' is found"
    it "should fail when no URL authority is present" $
      validateURL "http:" `shouldBe` Just "Authority not found in 'http:'"
    it "should fail when no domain is present" $
      validateURL "http://" `shouldBe` Just "Domain name not found in 'http://'"
    it "should fail when no dot is present in domain name" $
      validateURL "http://blah/bla.jpg" `shouldBe`
      Just "A dot must be present in domain name 'http://blah/bla.jpg'"
    it "should fail when on non image data url" $
      validateURL nonImgUrlShort `shouldBe`
      Just "Data URL must be an image 'data:someType;base64,blahblah'"
    it "should fail on long non image data url and crop the url" $
      validateURL nonImgUrl `shouldBe`
      Just "Data URL must be an image 'data:someType;base64,iVBORw0KG[...]'"
    it "should validate data URL" $
      validateURL imgUrl `shouldBe` Nothing
