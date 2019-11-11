{-# LANGUAGE FlexibleInstances #-}
import EAN13
import Control.Exception (assert)
import Data.Array ((!))
import Test.QuickCheck

rwhIsbn = [9,7,8,0,5,9,6,5,1,4,9,8,3]
-- https://en.wikipedia.org/wiki/File:EAN13.svg
ean13_1 = [5,9,0,1,2,3,4,1,2,3,4,5,7]
-- Purely Functional Data Structures (hardcover)
purelyFunctionalStructures1 = [9,7,8,0,5,2,1,6,3,1,2,4,2]
-- Purely Functional Data Structures (paperback)
purelyFunctionalStructures2 = [9,7,8,0,5,2,1,6,6,3,5,0,2]

newtype EAN13 a = EAN13 [a] deriving Show

instance Arbitrary (EAN13 Int) where
  arbitrary = elements [
    EAN13 rwhIsbn, EAN13 ean13_1, EAN13 ean13_2,
    EAN13 purelyFunctionalStructures1,
    EAN13 purelyFunctionalStructures2]

prop_checkDigit :: EAN13 Int -> Bool
prop_checkDigit (EAN13 is) = (checkDigit $ take 12 is) == last is

-- https://en.wikipedia.org/wiki/International_Article_Number#EAN-13_barcode_example
ean13_2 :: [Int]
ean13_2 = [4,0,0,3,9,9,4,1,5,5,4,8,6]
ean13_2_parts = [
  sideMarker       ,
  leftOddCodes  ! 0,
  leftEvenCodes ! 0,
  leftOddCodes  ! 3,
  leftOddCodes  ! 9,
  leftEvenCodes ! 9,
  leftEvenCodes ! 4,
  centerMarker     ,
  rightCodes    ! 1,
  rightCodes    ! 5,
  rightCodes    ! 5,
  rightCodes    ! 4,
  rightCodes    ! 8,
  rightCodes    ! 6,
  sideMarker]

t1 = assert
     ((encodeDigits $ take 12 ean13_2) == ean13_2_parts)
     "encodeDigits succeed"

tests = do
  quickCheck prop_checkDigit
  return t1
