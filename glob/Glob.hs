module Glob(namesMatching) where

import Data.Char(toLower)
import Data.List(sortOn)
import Control.Monad(filterM)
import GlobRegex
import System.Directory(doesPathExist, getDirectoryContents)
import System.FilePath
import System.IO.Error(catchIOError)

type NamesMatching = Either GlobError [FilePath]

isPattern = any (`elem` "*?[")

namesMatching :: String -> IO NamesMatching
namesMatching pat
  | isPattern pat = namesMatching' pat
  | otherwise = do
      e <- doesPathExist pat
      if (e)
        then return $ Right [pat]
        else return $ Right []

namesMatching' :: String -> IO NamesMatching
namesMatching' pat =
  case splitFileName pat of
    ("./",   filePat) -> filesMatching "." filePat
    (dirPat, filePat) -> do
      eitherDirs <- namesMatching $ dropTrailingPathSeparator dirPat
      case eitherDirs of
        Right dirs -> fmap (fmap concat . sequence)
                      $ mapM (`filesMatching` filePat) dirs
        Left e -> return $ Left e

filesMatching :: FilePath -> String -> IO NamesMatching
filesMatching dir filePat
  | isPattern filePat = listMatches dir filePat
  | otherwise = Right <$> listPlain dir filePat

isHidden ('.':_) = True
isHidden _ = False

listMatches :: FilePath -> String -> IO NamesMatching
listMatches dir filePat  = do
  files <- catchIOError
           (getDirectoryContents dir)
           (const $ return []) 
  return
    $ map (dir </>)
    <$> sortOn (dropWhile (`elem` ".#*") . map toLower)
    <$> filter hidden
    <$> filterM matches files
  where matches name
          | pathSeparator == '/' = matchesGlob False name filePat
          | otherwise = matchesGlob True name filePat
        hidden
          | isHidden filePat = isHidden
          | otherwise = not . isHidden

listPlain :: FilePath -> FilePath -> IO [FilePath]
listPlain dir file = do
  let path = dir </> file
  e <- doesPathExist path
  if (e)
    then return [path]
    else return []
