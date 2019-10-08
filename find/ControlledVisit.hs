module ControlledVisit where

import Control.Exception (bracket)
import Info
import LazyIO            (lazyForM)
import Data.List         (isPrefixOf, sort, sortBy)
import Data.Ord          (Down(..), comparing)
import System.Directory  (getModificationTime, getPermissions,
                          listDirectory, makeAbsolute,
                          pathIsSymbolicLink, searchable)
import System.FilePath   (FilePath, (</>))
import System.IO         (IOMode(ReadMode), openFile,
                          hFileSize, hClose)
import System.IO.Error   (catchIOError)
import System.Posix.Files(readSymbolicLink)

-- import System.Directory
-- import System.FilePath
-- myTraverse2 id (takeExtension <$>? infoPath ==?? ".hs" &&? infoSize >?? Just 1024) ".."
-- Finds all `.hs` files more than 1K in all subdirectories
-- fmap infoPath <$> myTraverse2 id (fmap searchable . infoPerms ==?? Just True ||? takeExtension <$>? infoPath ==?? ".hs" &&? infoSize >?? Just 1024) ".." >>= putStr . unlines

myTraverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
myTraverse order path = do
  names <- listDirectory path
  content <- fmap order $ mapM getInfo $ path:map (path </>) names
  fmap concat $ lazyForM content $ \info ->
    do
      cyclic <- isCyclicLink path
      if infoDirectory info &&
           infoPath info /= path &&
           not cyclic
         then myTraverse order $ infoPath info
         else return [info]

myTraverse2 order filterP = myTraverse (order . filter filterP)

getInfo :: FilePath -> IO Info
getInfo path = do
  perms <- maybePerms path
  size <- maybeSize path
  modTime <- maybeModTime path
  return $ Info path perms size modTime

maybeIO :: IO b -> IO (Maybe b)
maybeIO f = 
  catchIOError (Just <$> f) (\_ -> return Nothing)

maybePerms     = maybeIO . getPermissions
maybeSize file = maybeIO $ bracket (openFile file ReadMode) hClose hFileSize
maybeModTime   = maybeIO . getModificationTime

infoDirectory = maybe False searchable . infoPerms

cycleOnce l = tail $ take s $ cycle l where s = length l + 1
postOrder = cycleOnce
asc, desc :: [Info] -> [Info]
asc = sort
desc = sortBy (comparing Down)

-- Duplicate from `ExtGlob` with minor change
isCyclicLink path = do
  symlink <- pathIsSymbolicLink path
  if (symlink)
    then do
      targetPath <- readSymbolicLink path
      absolutTargetPath <- makeAbsolute targetPath
      return $ absolutTargetPath `isPrefixOf` absolutTargetPath
    else return False
