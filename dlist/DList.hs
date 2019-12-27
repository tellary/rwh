module DList where

newtype DList a = DList { unwrap :: [a] -> [a] }

instance Show a => Show (DList a) where
  show = show . toList

toList d = unwrap d []

add d e = DList $ unwrap d . (e:)

empty = DList id

fromList l = DList (l ++)

append d1 d2 = DList $ unwrap d1 . unwrap d2

cons x (DList xs) = DList $ (x:) . xs

instance Monoid (DList a) where
  mempty  = empty
  mappend = append

instance Foldable (DList) where
  foldr f z = foldr f z . toList
