module PPM where

import           Data.Array
                 (Array, listArray)
import qualified Data.ByteString.Lazy       as L
import           Data.List.Split (chunksOf)
import           Data.Word (Word8)
import           NetpbmCommon
                 (skipToNextBlock)
import qualified Parser                     as P
import           Text.Printf (printf)

type Pixel = Word8
type RGB = (Pixel, Pixel, Pixel)

data Pixmap = Pixmap {
  width  :: Int,
  height :: Int,
  depth  :: Int,
  pixmap :: Array (Int, Int) RGB
  }

instance Show Pixmap where
  show (Pixmap w h d _) = printf "Pixmap %i %i %i" w h d

pixmapArray :: Int -> Int -> [Pixel] -> Either String (Array (Int, Int) RGB)
pixmapArray w h d
  | 3*w*h /= l =
    Left
    $ printf "Data size doesn't match dimensions (3x(%ix%i) == %i) /= %i"
      w h (3*w*h) l
  | otherwise = Right $ listArray ((0,0), (w - 1, h - 1)) $ rgbs d
  where rgbs          = map rgb . chunksOf 3
        rgb [r, g, b] = (r, g, b)
        l             = length d

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
  case pixmapArray width height $ L.unpack bitmap of
    Right a -> return $ Pixmap width height depth a
    Left  e -> fail e

