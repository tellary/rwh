module PGM (convertPlainToRawPGM, pgm,
            plainPGM, plainPGMSlow,
            rawPGM, writeRawPGM, writeRawPGMIO) where

import           Control.Exception (bracket)
import           Control.Monad.Trans.Class
                 (lift)
import           Control.Monad.Trans.Except
                 (runExceptT, ExceptT(..))
import           Data.Array (elems, listArray, Array)
import           Data.Bits  (shift)
import qualified Data.ByteString.Lazy       as L
import           Data.Word (Word8)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char
                 (isDigit, isSpace)
import           NetpbmCommon
import qualified Parser                     as P
import           System.IO
                 (openFile, hClose,
                  hPutStr, IOMode(WriteMode))
import           Text.Printf (printf)

type Greymap = Image Pixel

greymapArray :: Int -> Int -> [Pixel] -> Either String (Array (Int, Int) Pixel)
greymapArray w h d
  | w*h /= l =
    Left
    $ printf "Data size doesn't match dimensions (%ix%i == %i) /= %i"
      w h (w*h) l
  | otherwise = Right $ listArray ((0,0), (h - 1, w - 1)) d
  where l = length d

greymap w h depth d = do
  case greymapArray w h d of
    Right a -> return $ Image depth a
    Left  e -> fail e

rawPGM = do
  header  <- P.takeWhileNotSpace
  P.assert "Invalid raw header" $ header == "P5"
  skipToNextBlock
  width   <- P.nat
  skipToNextBlock
  height  <- P.nat
  skipToNextBlock
  maxGrey <- P.nat
  P.assert
    ("Max grey must be less than 65536, but was " ++ show maxGrey) $
    maxGrey < 65536
  let bytesPerPixel = if maxGrey < 256 then 1 else 2
  skipToNextBlock
  let size   = width*height*bytesPerPixel
  let size64 = fromIntegral size
  bitmap     <- P.byteString  size
  P.assert
    ("Bitmap too short. " ++
     show width ++ "x" ++ show height ++ "=" ++ show (height*width) ++
     " bytes expected while " ++
     (show $ L.length bitmap) ++ " is found") $
    L.length bitmap == size64
  greymap width height maxGrey $ L.unpack bitmap

spanPlainPGMNat :: L.ByteString -> Either String (Int, Int, L.ByteString)
spanPlainPGMNat s =
  if natStr /= L.empty
  then
    case L8.uncons t2 of
      Just (h, _)
        | isSpace h -> Right (read $ L8.unpack natStr, consumed, t2)
        | otherwise -> Left "Not a digit"
      Nothing -> Right (read $ L8.unpack natStr, consumed, L.empty)
  else
    Left "Not a digit"
  where
    (spaces, t1) = L8.span isSpace s
    (natStr, t2) = L8.span isDigit t1
    consumed = fromIntegral $ L.length spaces + L.length natStr

spanPlainPGMData :: L.ByteString -> ([Int], Int, L.ByteString)
spanPlainPGMData s = loop ([], 0, s)
  where
    loop (is, c, s) =
      case spanPlainPGMNat s of
        Right (i, c', s') -> loop (i:is, c + c', s')
        Left _ -> (is, c, s)

plainPGMData = do
  s <- P.getState
  let (d, c, t) = spanPlainPGMData $ P.string s
  P.setState $ s { P.string = t, P.offset = P.offset s + c }
  return $ reverse d

plainPGMNat = do
  P.takeWhileSpace
  P.nat

plainPGMDataSlow size = P.take size plainPGMNat

plainPGMTemplate bitmapF = do
  header  <- P.takeWhileNotSpace
  P.assert "Invalid raw header" $ header == "P2"
  skipToNextBlock
  width   <- P.nat
  skipToNextBlock
  height  <- P.nat
  skipToNextBlock
  maxGrey <- P.nat
  P.assert
    ("Max grey must be less than 65536, but was " ++ show maxGrey) $
    maxGrey < 65536
  skipToNextBlock
  bitmap <- bitmapF (width*height)
  greymap width height maxGrey $ i2w =<< bitmap

i2w :: Int -> [Word8]
i2w i
  | i < 256   = [fromIntegral i]
  | i < 65536 = [fromIntegral $ shift i (-8), fromIntegral i]
  | otherwise = error "Only two-byte integers are supported"

plainPGM     = plainPGMTemplate $ \_ -> plainPGMData
plainPGMSlow = plainPGMTemplate plainPGMDataSlow

pgmTemplate plainPGMImpl = do
  header <- P.peek P.takeWhileNotSpace
  selectPGM header
  where
    selectPGM "P2" = plainPGMImpl
    selectPGM "P5" = rawPGM
    selectPGM h    = do fail $ "Unknown header '" ++ h ++ "'"

pgm = pgmTemplate plainPGM

writeRawPGM fn g = bracket (openFile fn WriteMode) hClose $ \f -> do
  hPutStr f "P5\n"
  hPutStr f $ show $ imageWidth  g
  hPutStr f " "
  hPutStr f $ show $ imageHeight g
  hPutStr f "\n"
  hPutStr f $ show $ imageDepth  g
  hPutStr f "\n"
  L.hPut  f $ L.pack $ elems $ imageData g

writeRawPGMIO :: FilePath -> ExceptT String IO Greymap -> ExceptT String IO ()
writeRawPGMIO f g = g >>= (lift . writeRawPGM f)

convertPlainToRawPGM i o = runExceptT $ do
  (g, _) <- P.parseIO plainPGM $ L.readFile i
  lift $ writeRawPGM o g
