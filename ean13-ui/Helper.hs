{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts #-}

module Helper (ean13, parseImage) where

import           Codec.Base64               (decode)
import           Codec.Picture              (imageData, imageHeight, imageWidth)
import           Codec.Picture              (Image, PixelRGB8, convertRGB8,
                                             decodeImage)
import           Data.Attoparsec.ByteString as P (IResult (Done, Fail, Partial),
                                                  Parser, parse, string,
                                                  takeByteString, takeWhile)
import           Data.ByteString            (ByteString, isInfixOf)
import           Data.Char                  (chr)
import qualified Data.Vector.Storable       as V (toList)
import           EAN13                      (findEAN13_0)
import           GHC.Stack                  (HasCallStack)
import           PPM                        (newPPM)

dataTypeP = P.takeWhile $ \w -> chr (fromIntegral w) /= ';'
dataUrlP :: Parser (ByteString, ByteString)
dataUrlP  = P.string "data:" *>
            ((,) <$> dataTypeP <* P.string ";base64," <*> P.takeByteString)

parseImage :: HasCallStack => ByteString -> Either String (Image PixelRGB8)
parseImage r =
  case parse dataUrlP r of
    Fail _ _ err -> Left $ err
    Partial cont -> let Done _ r = cont "" in doDecode r
    Done _ _     -> error "Done is unexpected with `dataUrlP`"
  where doDecode (dataType, b64)
          | "png"   `isInfixOf` dataType = Left "PNG images are not supported"
          | "image" `isInfixOf` dataType
          = fmap convertRGB8 $ decodeImage =<< bs
          | otherwise = Left "Not an image"
          where bs = decode b64

ean13 :: (HasCallStack, Integral d, Ord e, Fractional e)
      => Image PixelRGB8 -> Either String (e, [d])
ean13 img = do
  ppm <- newPPM (imageWidth img) (imageHeight img) 255
         . V.toList . imageData $ img
  maybe (Left $ "No EAN13 found") Right
    -- Scanning 3 rows, considering 3 candidate digits with
    -- a 55% black threshold. These parameters are manually tuned
    -- on files located in the `samples` folder.
    . findEAN13_0 3 3 0.55 $ ppm
