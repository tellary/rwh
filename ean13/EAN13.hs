module EAN13 where

-- https://en.wikipedia.org/wiki/International_Article_Number#Calculation_of_checksum_digit
checkDigit :: Integral a => [a] -> a
checkDigit digits = tenToZero $ 10 - sum products `mod` 10
  where products = zipWith ($) (cycle [(*3), (*1)]) $ reverse digits
        tenToZero 10 = 0
        tenToZero d  = d
