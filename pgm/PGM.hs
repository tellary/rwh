module PGM(convertPlainToRawPGM, plainPGM,
           rawPGM, writeRawPGM, writeRawPGMIO) where

import           Control.Monad.Trans.Except
                 (runExceptT, ExceptT(..))
import qualified Data.ByteString.Lazy       as L
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
  skipToNextBlock
  bitmap  <- P.byteString $ width*height
  let size64 = fromIntegral $ toInteger $ height*width
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
    $ L.pack $ map (fromInteger . toInteger) bitmap

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
