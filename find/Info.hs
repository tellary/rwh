module Info where

import Control.Monad    (liftM2)
import Data.Time.Clock  (UTCTime)
import System.Directory (Permissions, searchable)
import System.FilePath  (FilePath)

-- import System.FilePath
-- takeExtension <$>? infoPath ==?? ".cpp" &&? infoSize >?? Just 1024 $ Info "foo.C" Nothing (Just 2048) Nothing
-- takeExtension <$>? infoPath ==? (return ".cpp") &&? infoSize >? (return $ Just 1024) $ Info "foo.C" Nothing (Just 2048) Nothing

data Info = Info {
  infoPath    :: FilePath,
  infoPerms   :: Maybe Permissions,
  infoSize    :: Maybe Integer,
  infoModTime :: Maybe UTCTime
  } deriving (Eq, Ord, Show)

type InfoP a = Info -> a

searchableP :: InfoP (Maybe Bool)
searchableP = fmap searchable . infoPerms

liftM2' f m v = liftM2 f m (return v)

(<$>?) :: (a -> b) -> InfoP a -> InfoP b
(<$>?) = fmap

(&&?), (||?) :: InfoP Bool -> InfoP Bool -> InfoP Bool
(&&?) = liftM2 (&&)
(||?) = liftM2 (||)

(==??) :: Eq a => InfoP a -> a -> InfoP Bool
(==??) = liftM2' (==)
(>??), (<??) :: Ord a => InfoP a -> a -> InfoP Bool
(>??) = liftM2' (>)
(<??) = liftM2' (<)

(==?) :: Eq a => InfoP a -> InfoP a -> InfoP Bool
(==?) = liftM2 (==)
(>?), (<?) :: Ord a => InfoP a -> InfoP a -> InfoP Bool
(>?) = liftM2 (>)
(<?) = liftM2 (<)

infixr 9 <$>?
infixr 4 >??
infixr 4 <??
infixr 4 ==??
infixr 4 >?
infixr 4 <?
infixr 4 ==?
infixr 3 &&?
infixr 2 ||?
