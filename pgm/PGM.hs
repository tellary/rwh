module PGM(convertPlainToRawPGM, plainPGM,
           rawPGM, writeRawPGM, writeRawPGMIO) where

import           Control.Monad.Trans.Except
                 (runExceptT, ExceptT(..))
import           Data.Bits(shift)
import qualified Data.ByteString.Lazy       as L
import           Data.Word(Word8)
import           Control.Monad.Trans.Class
                 (lift)
import           Parser                     as P
import           System.IO (appendFile)

data Greymap = Greymap {
  greyWidth  :: Int,
  greyHeight :: Int,
  greyMax    :: Int,
  greyData   :: L.ByteString
  }

instance Show Greymap where
  show (Greymap w h m _) =
    "Greymap " ++ show w ++ "x" ++ show h ++ " " ++ show m

skipComment = P.takeWhile ((/= '\n')) char

skipToNextBlock = do
  takeWhileSpace
  c <- P.peek P.char
  if c == '#'
    then do
      skipComment
      skipToNextBlock
    else return ()

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
  let size = width*height*bytesPerPixel
  bitmap  <- P.byteString $ size
  let size64 = fromIntegral $ toInteger $ size
  P.assert
    ("Bitmap too short. " ++
     show width ++ "x" ++ show height ++ "=" ++ show (height*width) ++
     " bytes expected while " ++
     (show $ L.length bitmap) ++ " is found") $
    L.length bitmap == size64
  return $ Greymap width height maxGrey bitmap

plainPGMNat = do
  takeWhileSpace
  nat

plainPGM = do
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
  bitmap  <- P.take (width*height) plainPGMNat
  return $ Greymap width height maxGrey
    $ L.pack $ i2w =<< bitmap

i2w :: Int -> [Word8]
i2w i
  | i < 256   = [fromIntegral i]
  | i < 65536 = [fromIntegral $ shift i (-8), fromIntegral i]
  | otherwise = error "Only two-byte integers are supported"

writeRawPGM f g = do
  appendFile   f "P5\n"
  appendFile   f $ show $ greyWidth g
  appendFile   f " "
  appendFile   f $ show $ greyHeight g
  appendFile   f "\n"
  appendFile   f $ show $ greyMax g
  appendFile   f "\n"
  L.appendFile f $ greyData g

writeRawPGMIO :: FilePath -> ExceptT String IO Greymap -> ExceptT String IO ()
writeRawPGMIO f g = g >>= (lift . writeRawPGM f)

convertPlainToRawPGM i o = runExceptT $ do
  (g, _) <- parseIO plainPGM $ L.readFile i
  lift $ writeRawPGM o g
