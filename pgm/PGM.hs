module PGM(convertPlainToRawPGM, pgm,
           plainPGM, plainPGMSlow,
           rawPGM, writeRawPGM, writeRawPGMIO) where

import           Control.Monad.Trans.Except
                 (runExceptT, ExceptT(..))
import           Data.Bits(shift)
import qualified Data.ByteString.Lazy       as L
import           Data.Word(Word8)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char
                 (isDigit, isSpace)
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
  s <- getState
  let (d, c, t) = spanPlainPGMData $ string s
  setState $ s { string = t, offset = offset s + c }
  return $ reverse d

plainPGMNat = do
  takeWhileSpace
  nat

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
  return $ Greymap width height maxGrey
    $ L.pack $ i2w =<< bitmap

i2w :: Int -> [Word8]
i2w i
  | i < 256   = [fromIntegral i]
  | i < 65536 = [fromIntegral $ shift i (-8), fromIntegral i]
  | otherwise = error "Only two-byte integers are supported"

plainPGM     = plainPGMTemplate $ \_ -> plainPGMData
plainPGMSlow = plainPGMTemplate plainPGMDataSlow

pgmTemplate plainPGMImpl = do
  header <- peek P.takeWhileNotSpace
  selectPGM header
  where
    selectPGM "P2" = plainPGMImpl
    selectPGM "P5" = rawPGM
    selectPGM h    = do fail $ "Unknown header '" ++ h ++ "'"

pgm = pgmTemplate plainPGM

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
