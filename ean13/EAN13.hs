{-# LANGUAGE DeriveFunctor #-}

module EAN13 where

import           Control.Monad.Trans.Except (runExceptT)
import           Data.Array      ((!), elems, ixmap, listArray, Array, Ix)
import qualified Data.ByteString.Lazy as L
import           Data.List       (find, elemIndex, group, sortBy)
import           Data.List.Split (chunksOf)
import qualified Data.Map        as M
import           Data.Maybe      (listToMaybe)
import           Data.Ord        (comparing)
import           NetpbmCommon    (imageData, imageHeight, imageWidth)
import qualified Parser as P
import           PGM             (Greymap)
import           PPM             (ppm, Pixmap)
import           PPM2PGM         (ppmToPGM)

inv10 n = tenToZero $ 10 - n `mod` 10
  where tenToZero 10 = 0
        tenToZero d  = d

-- https://en.wikipedia.org/wiki/International_Article_Number#Calculation_of_checksum_digit
checkDigit :: Integral a => [a] -> a
checkDigit digits = inv10 $ sum products
  where products = zipWith ($) (cycle [(*3), (*1)]) $ reverse digits

-- A more mathematical version of checkDigit that allows to reason
-- about its recurrence better.
checkDigit1 digits =
  inv10 $ sum $ [3^((n - i) `mod` 2)*d | (i, d) <- zip [0 .. n - 1] digits]
  where n = length digits

-- Recurent formula for the check digit.
--
-- See EAN13QC(t10,t11) for test
checkDigitRecurrent d n c = inv10 $ 3^((n + 1) `mod` 2)*d + (inv10 c)

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

threshold :: (Ix k, Integral a, Ord r, Fractional r) =>
  r -> Array k a -> Array k Bit
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

scaledRuns :: Fractional b => [Run] -> [b]
scaledRuns rs = map divide rs
  where divide d = fromIntegral d / (fromIntegral $ sum rs)

scaledRuns1 :: (Eq a, Fractional b) => [a] -> [b]
scaledRuns1 = scaledRuns . runs

distanceSq a b = sum $ zipWith sqDelta a b
  where sqDelta x y = (x - y)^2

leftOddSRs, leftEvenSRs, rightSRs, paritySRs :: Fractional a => [[a]]
leftOddSRs  = map scaledRuns1 $ elems leftOddCodes
leftEvenSRs = map scaledRuns1 $ elems leftEvenCodes
rightSRs    = map scaledRuns1 $ elems rightCodes
paritySRs   = map scaledRuns1 $ elems parity

bestDigits :: (Fractional r, Ord r, Integral d) =>
  [[r]] -> [r] -> [(r, d)]
bestDigits digitSRs input =
  sortBy (comparing fst)
  $ zip digitScores digits
  where digits      = [0..9]
        digitScores = map (distanceSq $ input) digitSRs

data Parity a = Odd a | Even a | None a
                deriving (Show, Eq, Functor)

fromParity (Odd  a) = a
fromParity (Even a) = a
fromParity (None a) = a

parityToChar (Odd  _) = '1'
parityToChar (Even _) = '0'

type CandidateDigit r d = Parity (r, d)
type CandidatesForDigit r d = [CandidateDigit r d]
type CandidateDigits r d = [CandidatesForDigit r d]

bestLeft, bestRight, bestParity :: (Fractional r, Ord r, Integral d) =>
  [r] -> CandidatesForDigit r d
bestLeft   rs = sortBy (comparing (fst . fromParity)) $ odd ++ even
  where
    odd  = Odd  <$> bestDigits leftOddSRs  rs
    even = Even <$> bestDigits leftEvenSRs rs
bestRight  rs = None <$> bestDigits rightSRs  rs
bestParity rs = None <$> bestDigits paritySRs rs

candidateDigits :: (Eq a, Fractional r, Ord r, Integral d) =>
  Int -> [a] -> CandidateDigits r d
candidateDigits n xs =
  (take n . bestLeft <$> left) ++ (take n . bestRight <$> right)
  where left  = fmap scaledRuns $ take 6 $ chunksOf 4 $ drop 3  $ rs
        right = fmap scaledRuns $ take 6 $ chunksOf 4 $ drop 32 $ rs
        rs = runs xs

data Sequence e d = Sequence {
  sequenceCheckDigit :: d,
  sequenceError      :: e,
  sequenceDigits     :: [Parity d]
  } deriving Show

emptySequence :: (Integral d, Fractional e) => Sequence e d
emptySequence = Sequence 0 0 []

updateSequence :: (Integral d, Fractional e) =>
  Sequence e d -> CandidateDigit e d -> Sequence e d
updateSequence oldSeq candidateDigit =
  Sequence newCheckDigit newError $ newParityDigit:sequenceDigits oldSeq
  where newCheckDigit  = checkDigitRecurrent digit oldSeqLength oldCheckDigit
        oldCheckDigit  = sequenceCheckDigit oldSeq
        oldSeqLength   = length . sequenceDigits $ oldSeq
        (error, digit) = fromParity candidateDigit
        newError       = error + sequenceError oldSeq
        newParityDigit = snd <$> candidateDigit

type SequenceMap e d = M.Map d (Sequence e d)

emptySequenceMap :: (Integral d, Fractional e) => SequenceMap e d
emptySequenceMap = M.singleton 0 emptySequence

minErrorSequence s1 s2
  | sequenceError s1 < sequenceError s2 = s1
  | otherwise                           = s2

consumeCandidate :: (Integral d, Ord e, Fractional e) =>
  SequenceMap e d -> CandidateDigit e d -> SequenceMap e d -> SequenceMap e d
consumeCandidate oldMap candidateDigit newMap =
  M.foldrWithKey handleSeq newMap oldMap
  where handleSeq _ oldSeq newMap' =
          M.insertWith minErrorSequence newCheckDigit newSeq newMap'
          where newSeq        = updateSequence oldSeq candidateDigit
                newCheckDigit = sequenceCheckDigit newSeq

consumeCandidatesForDigit :: (Integral d, Ord e, Fractional e)
  => SequenceMap        e d
  -> CandidatesForDigit e d
  -> SequenceMap        e d
  -> SequenceMap        e d
consumeCandidatesForDigit oldMap candidatesForDigits newMap =
  foldr (consumeCandidate oldMap) newMap candidatesForDigits

consumeCandidates :: (Integral d, Ord e, Fractional e) =>
  CandidateDigits e d -> SequenceMap e d
consumeCandidates candidates =
  foldr handleDigit emptySequenceMap candidates
  where handleDigit candidatesForDigit oldMap =
          consumeCandidatesForDigit oldMap candidatesForDigit M.empty

digitSequencesByError :: (Integral d, Ord e, Fractional e) =>
  SequenceMap e d -> [Sequence e d]
digitSequencesByError = sortBy (comparing sequenceError) . fmap snd . M.assocs

solutionsByError :: (Integral d, Ord e, Fractional e) =>
  [Sequence e d] -> [[d]]
solutionsByError seqs =
  fmap fromParity <$> sequenceDigits <$> seqs

solutionsByError1 seqs = handleSeq <$> seqs
  where handleSeq seq =
          (sequenceError seq, fmap fromParity . sequenceDigits $ seq)

candidatesForFirstDigit :: (Integral d, Ord e, Fractional e) =>
  Int -> Sequence e d -> CandidatesForDigit e d
candidatesForFirstDigit n =
  take n . bestParity . scaledRuns1
  . fmap parityToChar . take 6 . sequenceDigits

addFirstDigitToSequence :: (Integral d, Ord e, Fractional e) =>
  Sequence e d -> Maybe (Sequence e d)
addFirstDigitToSequence seq =
  updateSequence seq . None . ((,) 0) . fromIntegral <$> idxMaybe
  where parityStr =
          fmap parityToChar . take 6 . sequenceDigits $ seq
        idxMaybe  = elemIndex parityStr $ elems parity

consumeFirstDigit :: (Integral d, Ord e, Fractional e) =>
  Int -> SequenceMap e d -> SequenceMap e d
consumeFirstDigit n map =
  foldr handleSeq M.empty map
  where handleSeq seq newMap =
          consumeCandidatesForDigit map candidates newMap
          where candidates = candidatesForFirstDigit n seq

appendCandidateCheckDigit :: (Integral d, Ord e, Fractional e) =>
  Sequence e d -> CandidateDigit e d -> Sequence e d
appendCandidateCheckDigit seq checkDigit =
  Sequence
  (sequenceCheckDigit seq)
  (sequenceError      seq +  fst (fromParity checkDigit))
  (sequenceDigits     seq ++ [snd <$> checkDigit])

appendCandidatesForCheckDigit :: (Integral d, Ord e, Fractional e) =>
  Sequence e d -> CandidatesForDigit e d -> [Sequence e d]
appendCandidatesForCheckDigit seq =
  fmap $ appendCandidateCheckDigit seq

consumeCandidatesForCheckDigit :: (Integral d, Ord e, Fractional e) =>
  CandidatesForDigit e d -> SequenceMap e d -> SequenceMap e d
consumeCandidatesForCheckDigit candidates map =
  M.foldrWithKey step M.empty map
  where step checkDigit seq newMap =
          case find (checkDigitEq checkDigit) candidates of
            Just candidate ->
              M.insert
              checkDigit
              (appendCandidateCheckDigit seq candidate)
              newMap
            Nothing        -> newMap
        checkDigitEq digit = (== digit) . snd . fromParity

solve0 :: (Eq a, Integral d, Ord e, Fractional e) =>
  Int -> [a] -> [Sequence e d]
solve0 n xs = digitSequencesByError m2
  where ds                    = candidateDigits n xs
        (candidates, checks') = splitAt 11 ds
        checks                = if null checks' then [] else head checks'
        m                     = consumeCandidates candidates
        m1                    = consumeFirstDigit n m
        m2                    = consumeCandidatesForCheckDigit checks m1

solve  n = solutionsByError  . solve0 n
solve1 n = solutionsByError1 . solve0 n

getRow :: (Ord t, Fractional t) => t -> Int -> Greymap -> Array Int Bit
getRow t n pgm = ixmap (0, maxWidthIdx) ((,) n) . threshold t $ pgmData
  where pgmData     = imageData  pgm
        maxWidthIdx = imageWidth pgm - 1

withRow :: (Ord t, Fractional t) => ([Bit] -> b) -> t -> Int -> Greymap -> b
withRow f t n =
  f
  -- A picture should have some white margin before
  -- start of the first the marker.
  . dropWhile (== Zero)
  -- A picture may be framed black. We're dropping the frame if exists.
  . dropWhile (== One)
  . elems . getRow t n

idxParts 0 _ = []
idxParts _ 0 = []
idxParts n h = fmap part [0..n']
  where part i = i*(h + 1) `div` (n' + 1)
        n' = min n h

solvePGM0 :: (Integral d, Ord e, Fractional e)
  => Int -- number of rows from image to take
  -> Int -- number of candidate digits in each raw
  -> e   -- black/white bit threshold
  -> Greymap -> [Sequence e d]
solvePGM0 r n t pgm =
  sortBy (comparing sequenceError) $ solveRow =<< idxParts r (h - 1)
  where h           = imageHeight pgm
        solveRow  r = withRow (solve0 n) t r pgm

findEAN13_0 r n t = listToMaybe . solutionsByError1 . solvePGM0 r n t . ppmToPGM

findEAN13 :: (Integral d, Ord e, Fractional e) => Pixmap -> Maybe (e, [d])
findEAN13 = findEAN13_0 3 3 0.6

readEAN13File0 :: (Integral d, Ord e, Fractional e)
  => Int
  -> Int
  -> e
  -> FilePath -> IO (Either String (e, [d]))
readEAN13File0 r n t f = do
  ppmFileE <- runExceptT ppmFileT
  return $ do
    ppmFile <- ppmFileE
    maybe (Left $ "No EAN13 found in " ++ f) Right $ findEAN13 ppmFile
  where ppmFileT = fmap fst . P.parseIO ppm $ L.readFile f
