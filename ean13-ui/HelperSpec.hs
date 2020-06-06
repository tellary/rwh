import           Codec.Picture   (convertRGB8, decodeImage)
import qualified Data.ByteString as B
import           Helper          (ean13, errorCutoff)
import           Test.Hspec      (describe, hspec, it, shouldReturn)

fileEan13 :: FilePath -> IO (Either String (Double, [Int]))
fileEan13 f = do
  bs <- B.readFile f
  let img = fmap convertRGB8 $ decodeImage bs
  return $ ean13 =<< img

goodFileEan13 :: FilePath -> IO (Maybe [Int])
goodFileEan13 f = do
  e <- fileEan13 f
  case e of
    Right (e, ds) | e < errorCutoff -> return . Just $ ds
    _                               -> return Nothing

goodFiles =
  [ ("ean13_1.jpg" , [1,2,3,4,5,6,7,8,9,0,1,2,8])
  , ("ean13_3.jpg" , [0,7,0,5,6,3,2,0,8,5,9,4,3])
  , ("ean13_4.jpg" , [3,8,0,0,0,6,5,7,1,1,1,3,5])
  , ("ean13_5.jpg" , [4,9,0,2,5,0,6,3,0,4,9,1,9])
  , ("ean13_6.jpeg", [0,0,7,2,5,1,2,0,4,4,9,0,2])
  , ("ean13_7.jpeg", [8,7,1,8,8,6,8,6,6,9,0,7,0])
  ]

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
