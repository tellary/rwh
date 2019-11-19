module PPM (newPPM, ppm, Pixel, Pixmap, RGB) where

import           Data.Array
                 (Array, listArray)
import qualified Data.ByteString.Lazy       as L
import           Data.List.Split (chunksOf)
import           NetpbmCommon
import qualified Parser                     as P
import           Text.Printf (printf)

type RGB = (Pixel, Pixel, Pixel)
type Pixmap = Image RGB

rgbmapArray :: Int -> Int -> [Pixel] -> Either String (Array (Int, Int) RGB)
rgbmapArray w h d
  | 3*w*h /= l =
    Left
    $ printf "Data size doesn't match dimensions (3x(%ix%i) == %i) /= %i"
      w h (3*w*h) l
  | otherwise = Right $ listArray ((0,0), (w - 1, h - 1)) $ rgbs d
  where rgbs          = map rgb . chunksOf 3
        rgb [r, g, b] = (r, g, b)
        rgb _         = error
          $ "RGB data must be multiple of 3. " ++
            "This error shouldn't be possible as per `rgbmapArray` validation."
        l             = length d

newPPM w h depth d = Image depth <$> rgbmapArray w h d

ppm = do
  header  <- P.takeWhileNotSpace
  P.assert
    ("Invalid raw header expected P6, but found " ++ header) $
    header == "P6"
  skipToNextBlock
  width   <- P.nat
  skipToNextBlock
  height  <- P.nat
  skipToNextBlock
  depth <- P.nat
  P.assert
    ("Depth must be 255, but was " ++ show depth) $
    depth == 255
  skipToNextBlock
  let size = 3*width*height
  bitmap <- P.byteString size
  case newPPM width height depth $ L.unpack bitmap of
    Right i -> return $ i
    Left  e -> fail e

