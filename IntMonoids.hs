{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IntMonoids where

newtype AInt = AInt Int deriving (Num, Show)
newtype MInt = MInt Int deriving (Num, Show)

instance Monoid (AInt) where
  mempty  = 0
  mappend = (+)

instance Monoid (MInt) where
  mempty  = 1
  mappend = (*)
