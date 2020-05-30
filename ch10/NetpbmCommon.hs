module NetpbmCommon(imageData, imageDepth, imageHeight, imageWidth,
                    skipToNextBlock, Image(..), Pixel) where

import           Data.Array  (bounds, Array)
import           Data.Word   (Word8)
import qualified Parser      as P
import           Text.Printf (printf)

type Pixel = Word8

data Image a = Image Int (Array (Int, Int) a)

imageData   (Image _ a) = a
imageDepth  (Image d _) = d
imageHeight (Image _ a) = (fst $ snd $ bounds a) + 1
imageWidth  (Image _ a) = (snd $ snd $ bounds a) + 1

instance Show (Image a) where
  show i@(Image d _) = printf "Image %i %i %i" w h d
    where w = imageWidth  i
          h = imageHeight i

skipComment = P.takeWhile ((/= '\n')) P.char

skipToNextBlock = do
  P.takeWhileSpace
  c <- P.peek P.char
  if c == '#'
    then do
      skipComment
      skipToNextBlock
    else return ()
