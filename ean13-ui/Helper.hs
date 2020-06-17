{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Helper
  ( createDataUrl
  , ean13
  , errorCutoff
  , parseImageByteString
  , parseImageDataUrl) where

import           Codec.Base64               (decode, encode)
import           Codec.Picture              (Image, PixelRGB8, convertRGB8,
                                             decodeImage, decodeJpeg, imageData,
                                             imageHeight, imageWidth)
import           Data.Attoparsec.ByteString as P (IResult (Done, Fail, Partial),
                                                  Parser, parse, string,
                                                  takeByteString, takeWhile)
import           Data.ByteString            (ByteString, append, isPrefixOf)
import           Data.Char                  (chr)
import qualified Data.Vector.Storable       as V (toList)
import           EAN13                      (findEAN13_0)
import           GHC.Stack                  (HasCallStack)
import           Model                      (EAN13 (EAN13))
import           PPM                        (newPPM)

dataTypeP = P.takeWhile $ \w -> chr (fromIntegral w) /= ';'
dataUrlP :: Parser (ByteString, ByteString)
dataUrlP  = P.string "data:" *>
            ((,) <$> dataTypeP <* P.string ";base64," <*> P.takeByteString)

decoders
  = [("jpeg", decodeJpeg)]

parseImageDataUrl :: HasCallStack
  => ByteString -> Either String (Image PixelRGB8)
parseImageDataUrl r =
  case parse dataUrlP r of
    Fail _ _ err -> Left $ err
    Partial cont -> let Done _ r = cont "" in doDecode r
    Done _ _     -> error "Done is unexpected with `dataUrlP`"
  where doDecode (dataType, b64)
          | "image/png" == dataType
          = Left "PNG images are not supported"
          | "image/" `isPrefixOf` dataType
          = fmap convertRGB8 $ decodeImage =<< bs
          | otherwise = Left "Not an image"
          where bs = decode b64

parseImageByteString :: HasCallStack
  => ByteString -> Either [(String, String)] (String, Image PixelRGB8)
parseImageByteString bs
  = foldr decode (Left []) decoders
  where decode (t, d) r
          = case d bs of
              Right i   -> Right (t, convertRGB8 i)
              Left  err -> case r of
                Left errs   -> Left ((t, err):errs)
                r@(Right _) -> r

createDataUrl t bs
  = "data:image/" `append` t
  `append` ";base64," `append` encode bs

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
