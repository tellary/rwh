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
filesMatching dir filePat@('.':_) = filesMatching' dir filePat
filesMatching dir filePat         = filesMatching' dir ("[!.]" ++ filePat)

filesMatching' dir filePat = do
  files <- catchIOError
           (getDirectoryContents dir)
           (const $ return []) 
  return
    $ map (dir </>)
    $ sortOn (dropWhile (`elem` ".#*") . map toLower)
    $ filter matches files
  where matches name = matchesGlob False name filePat
