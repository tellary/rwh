module EAN13 where

import Data.Array ((!), elems, listArray)

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
  | any ((< 0)) is  = Nothing
  | any ((> 9)) is  = Nothing
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
