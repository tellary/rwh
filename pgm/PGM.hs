module PGM(parseRawPGM) where

import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Parser                     as P

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

parseRawPGM = do
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

parsePlainPGM = do
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
