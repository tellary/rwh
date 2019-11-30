{-# LANGUAGE FlexibleInstances #-}

import           Data.Ratio ((%))
import           EAN13
import           Control.Exception (assert)
import           Control.Monad.Trans.Except (runExceptT)
import           Data.Array ((!), listArray)
import qualified Data.ByteString.Lazy as L
import           Test.QuickCheck
import qualified Parser as P
import           NetpbmCommon
import           PPM
import           PPM2PGM

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
t6 = assert
     ((encodeEAN13 $ take 12 ean13_2) == (Just $ concat ean13_2_parts))
     "encodeEAN13 succeed"

whiteSqPPM = fmap fst $ P.parseIO ppm $ L.readFile "../netpbm/white_sq.ppm"
whiteSqPGM = ppmToPGM <$> whiteSqPPM

whiteSqThreshold = threshold 0.5 <$> imageData <$> whiteSqPGM

expectedWhiteSqThreshold = listArray ((0,0), (3,3))
  [One,  One,  One, One,
   One, Zero, Zero, One,
   One, Zero, Zero, One,
   One,  One,  One, One]

t2 = do
  w <- whiteSqThreshold
  return $ assert (expectedWhiteSqThreshold == w) "expectedWhiteSqThreshold"

t3 = assert ((runs $ leftOddCodes!0) == [3,2,1,1])
     "Expected leftOddCodes!0 runs"

t4 = assert ((scaledRuns1 $ leftOddCodes!0) == [3 % 7,2 % 7,1 % 7,1 % 7])
     "Expected leftOddCodes!0 normalizedRuns"

t5 = assert
     ((head $ bestDigits rightSRs $ scaledRuns1 $ rightCodes!2) == (0.0, 2))
     "Second right code expected to be the best among right runs"

t7 = assert
     ((fmap head $ candidateDigits 1 $ concat ean13_2_parts) ==
      ([Odd  (0.0, 0), Even (0.0, 0), Odd  (0.0, 3),
        Odd  (0.0, 9), Even (0.0, 9), Even (0.0, 4),
        None (0.0, 1), None (0.0, 5), None (0.0, 5),
        None (0.0, 4), None (0.0, 8), None (0.0, 6)]))
     "Best candidate digits of a correct EAN13 encoding are expected " ++
     "and have 0.0 error"

seqDigits = listArray (0,3) [
  None (2 % 49,8), None (0 % 1,8), Odd (2 % 49,2), Even (0 % 1,0)]

seq1 = updateSequence emptySequence $ seqDigits!0
seq2 = updateSequence seq1 $ seqDigits!1
seq3 = updateSequence seq2 $ seqDigits!2
seq4 = updateSequence seq3 $ seqDigits!3

t8 = assert (expectedSeq4CheckDigit == actualSeq4CheckDigit)
     "expectedSeq4CheckDigit"
  where expectedSeq4CheckDigit =
          checkDigit $ fromParity <$> sequenceDigits seq4
        actualSeq4CheckDigit   = sequenceCheckDigit seq4

expectedSeq4Error = sum . fmap fst . fmap fromParity $ seqDigits
t9 = assert (expectedSeq4Error == actualSeq4Error)
     "expectedSeq4Error"
  where actualSeq4Error   = sequenceError seq4

t10 = assert
      ((checkDigitRecurrent 8 (length [1,3]) $ checkDigit [1,3]) ==
       checkDigit [8,1,3])
      "checkDigitRecurrent 8 (length [1,3]) 0"

t11 = assert
      ((checkDigitRecurrent 4 (length [8,1,3]) $ checkDigit [8,1,3]) ==
       checkDigit [4,8,1,3])
      "checkDigitRecurrent 4 (length [8,1,3])"

ean13_2_xs = concat ean13_2_parts
ean13_2_ds = candidateDigits 3 ean13_2_xs :: [[Parity (Rational, Int)]]

tests = do
  quickCheck prop_checkDigit
  t2' <- runExceptT t2
  return [
    return t1,
    return t6,
    t2',
    return t3,
    return t4,
    return t5,
    return t7,
    return t8,
    return t9,
    return t10,
    return t11]
