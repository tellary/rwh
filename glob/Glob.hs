module Glob(namesMatching) where

import Data.Char(toLower)
import Data.List(sortOn)
import GlobRegex
import System.Directory(doesPathExist, getDirectoryContents)
import System.FilePath
import System.IO.Error(catchIOError)

isPattern = any (`elem` "*?[")

namesMatching :: String -> IO [String]
namesMatching pat
  | isPattern pat = namesMatching' pat
  | otherwise = do
      e <- doesPathExist pat
      if (e)
        then return [pat]
        else return []

namesMatching' pat =
  case splitFileName pat of
    ("./",   filePat) -> filesMatching "." filePat
    (dirPat, filePat) -> do
      dirs <- namesMatching $ dropTrailingPathSeparator dirPat
      fmap concat $ mapM (`filesMatching` filePat) dirs


filesMatching :: FilePath -> String -> IO [FilePath]
filesMatching dir filePat
  | isPattern filePat = listMatches dir filePat
  | otherwise = listPlain dir filePat

isHidden ('.':_) = True
isHidden _ = False

listMatches dir filePat  = do
  files <- catchIOError
           (getDirectoryContents dir)
           (const $ return []) 
  return
    $ map (dir </>)
    $ sortOn (dropWhile (`elem` ".#*") . map toLower)
    $ filter hidden
    $ filter matches files
  where matches name
          | pathSeparator == '/' = matchesGlob False name filePat
          | otherwise = matchesGlob True name filePat
        hidden
          | isHidden filePat = isHidden
          | otherwise = not . isHidden

listPlain dir file = do
  let path = dir </> file
  e <- doesPathExist path
  if (e)
    then return [path]
    else return []
