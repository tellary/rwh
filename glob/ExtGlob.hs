module Glob(namesMatching) where

import Data.Char(toLower)
import Data.List(isInfixOf, sortOn)
import GlobRegex
import System.Directory(doesPathExist, getDirectoryContents)
import System.FilePath
import System.IO.Error(catchIOError)

isPattern = any (`elem` "*?[")
isExtPattern = ("**" `isInfixOf`)

namesMatching :: String -> IO [FilePath]
namesMatching =
  namesMatchingSplit ""
  . (map dropTrailingPathSeparator) . splitPath

namesMatchingSplit :: FilePath -> [String] -> IO [FilePath]
namesMatchingSplit dir [] = return [dir]
namesMatchingSplit dir (pat:patParts)
  | isExtPattern pat = undefined
  | otherwise = do
      paths <- listDir dir pat
      subpaths paths patParts

listDir :: FilePath -> String -> IO [FilePath]
listDir dir pat
  | isPattern pat = listMatches dir pat
  | otherwise = listPlain dir pat

subpaths :: [FilePath] -> [String] -> IO [FilePath]
subpaths paths patParts =
  concat <$> mapM pathAndSubpaths paths
  where
    pathAndSubpaths path = do
      subpaths <- namesMatchingSplit path patParts
      return subpaths

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
