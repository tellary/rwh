{-# LANGUAGE FlexibleInstances #-}
import EAN13
import Test.QuickCheck

rwhIsbn = [9,7,8,0,5,9,6,5,1,4,9,8,3]
-- https://en.wikipedia.org/wiki/File:EAN13.svg
ean13_1 = [5,9,0,1,2,3,4,1,2,3,4,5,7]
-- Purely Functional Data Structures (hardcover)
purelyFunctionalStructures1 = [9,7,8,0,5,2,1,6,3,1,2,4,2]
-- Purely Functional Data Structures (paperback)
purelyFunctionalStructures2 = [9,7,8,0,5,2,1,6,6,3,5,0,2]

newtype EAN13 a = EAN13 [a] deriving Show

instance Arbitrary (EAN13 Integer) where
  arbitrary = elements [
    EAN13 rwhIsbn, EAN13 ean13_1,
    EAN13 purelyFunctionalStructures1,
    EAN13 purelyFunctionalStructures2]

prop_checkDigit (EAN13 is) = (checkDigit $ take 12 is) == last is
