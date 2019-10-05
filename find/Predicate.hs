module Predicate where

import System.Directory(Permissions)
import System.FilePath(FilePath)
import System.Time(ClockTime)

-- import System.FilePath
-- takeExtension <$>? pathP ==?? ".cpp" &&? sizeP >?? 1024
-- takeExtension <$>? pathP ==? liftP0 ".cpp" &&? sizeP >? liftP0 1024

type InfoP a =
  FilePath ->
  Permissions ->
  Maybe Int ->
  ClockTime -> a

pathP :: InfoP FilePath
pathP p _ _ _ = p

permP :: InfoP Permissions
permP _ p _ _ = p

sizeP :: InfoP Int
sizeP _ _ (Just s) _ = s
sizeP _ _ Nothing  _ = -1

createdP :: InfoP ClockTime
createdP _ _ _ c = c

-- Analog of `return` or `pure`
liftP0 :: a -> InfoP a 
liftP0 a _ _ _ _ = a

liftP1 :: (a -> b) -> InfoP a -> InfoP b
liftP1 q i f p s c = q $ i f p s c

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q i1 i2 f p s c = i1 f p s c `q` i2 f p s c

liftP2' q i v = liftP2 q i (liftP0 v)
                           
(<$>?) = liftP1

(&&?), (||?) :: InfoP Bool -> InfoP Bool -> InfoP Bool
(&&?) = liftP2 (&&)
(||?) = liftP2 (||)

infixr 9 <$>?
infixr 4 >??
infixr 4 <??
infixr 4 ==??
infixr 4 >?
infixr 4 <?
infixr 4 ==?
infixr 3 &&?
infixr 3 ||?  

(==??) :: Eq a => InfoP a -> a -> InfoP Bool
(==??) = liftP2' (==)
(>??), (<??) :: Ord a => InfoP a -> a -> InfoP Bool
(>??) = liftP2' (>)
(<??) = liftP2' (<)

(==?) :: Eq a => InfoP a -> InfoP a -> InfoP Bool
(==?) = liftP2 (==)
(>?), (<?) :: Ord a => InfoP a -> InfoP a -> InfoP Bool
(>?) = liftP2 (>)
(<?) = liftP2 (<)
