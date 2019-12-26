module DList where

import Data.List (foldl')

newtype DList a = DList { unwrap :: [a] -> [a] }

instance Show a => Show (DList a) where
  show = show . toList

toList d = unwrap d []

add d e = DList $ unwrap d . (e:)

empty = DList id

fromList :: Foldable t => t a -> DList a
fromList = foldl' add empty

append d1 d2 = DList $ unwrap d1 . unwrap d2

instance Monoid (DList a) where
  mempty  = empty
  mappend = append
