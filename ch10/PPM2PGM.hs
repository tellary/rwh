module PPM2PGM where

import           Control.Monad.Trans.Class
                 (lift)
import           Control.Monad.Trans.Except
                 (runExceptT)
import qualified Data.ByteString.Lazy       as L
import           NetpbmCommon
import qualified Parser                     as P
import           PGM
import           PPM
import           Text.Printf (printf)

luminace :: RGB -> Pixel
luminace (r, g, b) =
  round255 $ r'*0.30 + g'*0.59 + b'*0.11
  where [r', g', b'] = map fromIntegral [r, g, b]
        round255 x
          | x > 255.0 = error
            $ printf
              "(255,255,255) expect max (R,G,B) value, but was (%i,%i,%i)"
              r g b
          | otherwise = round x

-- import qualified Data.ByteString.Lazy as L
-- import qualified Parser as P
-- import PPM
-- import Data.Array
-- px = fmap fst $ P.parseIO ppm $ L.readFile "../netpbm/ean13_2.ppm"
-- runExceptT $ ixmap ((0,0), (0,5)) id <$> imageData <$> ppmToPGM <$> px
ppmToPGM :: Image RGB -> Image Pixel
ppmToPGM ppm = Image (imageDepth ppm) $ fmap luminace $ imageData ppm

convertPPMToPGM i o = runExceptT $ do
  (i, _) <- P.parseIO ppm $ L.readFile i
  let g = ppmToPGM i
  lift $ writeRawPGM o g
