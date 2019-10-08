module ControlledVisit where

import Control.Monad    (forM)
import System.Directory (Permissions, getPermissions,
                         listDirectory, searchable)
import System.FilePath  (FilePath, (</>))
import System.IO.Error  (catchIOError)
import System.Time      (ClockTime)

data Info = Info {
  infoPath    :: FilePath,
  infoPerms   :: Maybe Permissions,
  infoSize    :: Maybe Int,
  infoModTime :: Maybe ClockTime
  } deriving (Eq, Ord, Show)

myTraverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
myTraverse order path = do
  names <- listDirectory path
  content <- fmap order $ mapM getInfo $ path:map (path </>) names
  fmap concat $ forM content $ \info ->
    do if infoDirectory info && infoPath info /= path
         then myTraverse order $ infoPath info
         else return [info]

getInfo :: FilePath -> IO Info
getInfo path = do
  perms <- maybePerms path
  return $ Info path perms Nothing Nothing

maybeIO :: IO b -> IO (Maybe b)
maybeIO f = 
  catchIOError (Just <$> f) (\_ -> return Nothing)
maybePerms = maybeIO . getPermissions

infoDirectory = maybe False searchable . infoPerms

cycleOnce l = tail $ take s $ cycle l where s = length l + 1
