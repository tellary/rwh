module EAN13 where

import Data.Array ((!), elems, listArray, Array, Ix)
import Data.List  (group, sortBy)
import Data.Ord   (comparing)

-- https://en.wikipedia.org/wiki/International_Article_Number#Calculation_of_checksum_digit
checkDigit :: Integral a => [a] -> a
checkDigit digits = tenToZero $ 10 - sum products `mod` 10
  where products = zipWith ($) (cycle [(*3), (*1)]) $ reverse digits
        tenToZero 10 = 0
        tenToZero d  = d

array xs = listArray (0, l - 1) xs
  where l = length xs

leftOddCodes = array [
  "0001101", "0011001", "0010011", "0111101", "0100011",
  "0110001", "0101111", "0111011", "0110111", "0001011"]

rightCodes = fmap compliment <$> leftOddCodes
  where compliment '0' = '1'
        compliment '1' = '0'

parity = array [
  "111111", "110100", "110010", "110001", "101100",
  "100110", "100011", "101010", "101001", "100101"]

leftEvenCodes = array $ reverse <$> elems rightCodes

sideMarker   = "101"
centerMarker = "01010"

encodeEAN13 :: [Int] -> Maybe String
encodeEAN13 is
  | length is /= 12 = Nothing
  | any (< 0) is    = Nothing
  | any (> 9) is    = Nothing
  | otherwise       = Just $ concat $ encodeDigits is

encodeDigits :: [Int] -> [String]
encodeDigits c@(first:is) =
  sideMarker:leftEncoded ++ centerMarker:rightEncoded ++ [sideMarker]
  where
    (left, right) = splitAt 6 is
    leftEncoded   = zipWith encodeLeft (parity ! first) left
    rightEncoded  = encodeRight <$> right ++ [checkDigit c]

encodeLeft '0' = (leftEvenCodes !)
encodeLeft '1' = (leftOddCodes  !)

encodeRight d = rightCodes ! d

data Bit = Zero | One deriving (Eq, Show)

threshold :: (Ix k, Integral a) => Double -> Array k a -> Array k Bit
threshold r a = binary <$> a
  where binary m
          --   0 pixel represents black
          | fromIntegral m < pivot = One
          -- 255 pixel represents white
          | otherwise = Zero
        pivot = r*(max - min) + min
        max   = fromIntegral $ maximum a
        min   = fromIntegral $ minimum a

type Run  = Int
type RunElems a = [(Run, a)]

runElems :: Eq a => [a] -> RunElems a
runElems = map runElem . group
  where runElem gr = (length gr, head gr)

runs :: Eq a => [a] -> [Run]
runs = fmap fst . runElems

scaledRuns :: (Eq a, Fractional b) => [a] -> [b]
scaledRuns xs = map divide rs
  where divide d = fromIntegral d / (fromIntegral $ sum rs)
        rs = runs xs

distanceSq a b = sum $ zipWith sqDelta a b
  where sqDelta x y = (x - y)^2

leftOddRuns, leftEvenRuns, rightRuns :: Fractional a => [[a]]
leftOddRuns  = map scaledRuns $ elems leftOddCodes
leftEvenRuns = map scaledRuns $ elems leftEvenCodes
rightRuns    = map scaledRuns $ elems rightCodes

bestDigits :: (Eq a, Ord r, Fractional r, Integral d) =>
  [[r]] -> [a] -> [(r, d)]
bestDigits digitRuns input =
  sortBy (comparing fst)
  $ zip digitScores digits
  where digits      = [0..9]
        digitScores = map (distanceSq $ scaledRuns input) digitRuns
