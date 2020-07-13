{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Helper
  ( ean13
  , errorCutoff
  , formatBarcode
  , parseImageDataUrl
  , validateURL) where

import           Codec.Base64               (decode)
import           Codec.Picture              (Image, PixelRGB8, convertRGB8,
                                             decodeImage, imageData,
                                             imageHeight, imageWidth)
import           Control.Monad              (unless)
import           Data.Attoparsec.ByteString as P (IResult (Done, Fail, Partial),
                                                  Parser, parse, string,
                                                  takeByteString, takeWhile)
import           Data.ByteString            (ByteString, isPrefixOf)
import           Data.Char                  (chr)
import           Data.List                  (isInfixOf)
import qualified Data.Vector.Storable       as V (toList)
import           EAN13                      (findEAN13_0)
import           GHC.Stack                  (HasCallStack)
import           Model                      (EAN13 (EAN13))
import           Network.URI                (URI (uriAuthority, uriScheme),
                                             URIAuth (uriRegName), parseURI)
import           PPM                        (newPPM)
import           Text.Printf                (printf)

dataTypeP = P.takeWhile $ \w -> chr (fromIntegral w) /= ';'
dataUrlP :: Parser (ByteString, ByteString)
dataUrlP  = P.string "data:" *>
            ((,) <$> dataTypeP <* P.string ";base64," <*> P.takeByteString)

parseImageDataUrl :: HasCallStack
  => ByteString -> Either String (Image PixelRGB8)
parseImageDataUrl r =
  case parse dataUrlP r of
    Fail _ _ err -> Left $ err
    Partial cont -> let Done _ r = cont "" in doDecode r
    Done _ _     -> error "Done is unexpected with `dataUrlP`"
  where doDecode (dataType, b64)
          | "image/png" == dataType
          = Left "PNG images are not supported by JuicyPixels in a Browser"
          | "image/" `isPrefixOf` dataType
          = fmap convertRGB8 $ decodeImage =<< bs
          | otherwise = Left "Not an image"
          where bs = decode b64

ean13 :: HasCallStack
  => Image PixelRGB8 -> Either String EAN13
ean13 img = do
  ppm <- newPPM (imageWidth img) (imageHeight img) 255
         . V.toList . imageData $ img
  maybe (Left $ "No EAN13 found") (Right . mkEAN13)
    -- Scanning 3 rows, considering 3 candidate digits with
    -- a 55% black threshold. These parameters are manually tuned
    -- on files located in the `samples` folder.
    . findEAN13_0 3 3 0.55 $ ppm
  where mkEAN13 (e, ds) = EAN13 ds e

-- 14.25 extra lines in a barcode of 95 lines
errorCutoff = 0.15

-- Quick and dirty rather than "well implemented" and type safe
formatBarcode :: [Int] -> (String, Maybe String)
formatBarcode [d01, d02, d03, d04, d05, d06, d07, d08, d09, d10, d11, d12, d13]
  = (ean13, upc)
  where upc
          | d01 == 0
          = Just
            $ printf "%i %i%i%i%i%i %i%i%i%i%i %i"
              d02 d03 d04 d05 d06 d07 d08 d09 d10 d11 d12 d13
          | otherwise = Nothing
        ean13
          = printf "%i %i%i%i%i%i%i %i%i%i%i%i%i"
            d01 d02 d03 d04 d05 d06 d07 d08 d09 d10 d11 d12 d13
formatBarcode ds = error $ "Unexpected barcode length: " ++ show ds

validateURL :: String -> Maybe String
validateURL str =
  case doValidateURL of
    Left err -> Just err
    Right () -> Nothing
  where
    doValidateURL = do
      u <- case parseURI str of
             Nothing -> Left $ printf "Can't parse '%s' as URI" str
             Just u -> return u
      let s = uriScheme u
      unless (s `elem` ["http:", "https:"])
        . Left
        $ printf "'http:' or 'https:' scheme is expected but '%s' is found" s
      case uriAuthority $ u of
        Nothing -> Left $ printf "Authority not found in '%s'" str
        Just a  -> do
          let domain = uriRegName $ a
          unless (not . null $ domain)
            . Left $ printf "Domain name not found in '%s'" str
          unless ("." `isInfixOf` domain)
            . Left $ printf "A dot must be present in domain name '%s'" str
