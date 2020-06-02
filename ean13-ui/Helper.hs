{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts #-}

module Helper where

import Codec.Base64               (decode)
import Codec.Picture              (Image, PixelRGB8, convertRGB8, decodeImage)
import Data.Attoparsec.ByteString as P
import Data.ByteString            (ByteString, isInfixOf)
import Data.Char                  (chr)
import GHC.Stack                  (HasCallStack)

dataTypeP = P.takeWhile $ \w -> chr (fromIntegral w) /= ';'
dataUrlP :: Parser (ByteString, ByteString)
dataUrlP  = P.string "data:" *> ((,) <$> dataTypeP <* P.string ";base64," <*> P.takeByteString)

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
