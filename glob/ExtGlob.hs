module ExtGlob(namesMatching) where

import Control.Monad(filterM)
import Data.Char(toLower)
import Data.List(isInfixOf, isPrefixOf, sortOn)
import GlobRegex
import System.Directory(
  doesPathExist, getDirectoryContents, listDirectory,
  makeAbsolute, pathIsSymbolicLink)
import System.FilePath
import System.IO.Error(catchIOError)
import System.IO.Unsafe(unsafeInterleaveIO)
import System.Posix.Files(readSymbolicLink)

isPattern = any (`elem` "*?[")
isExtPattern = ("**" `isInfixOf`)

namesMatching :: String -> IO [FilePath]
namesMatching =
  namesMatchingSplit ""
  . (map dropTrailingPathSeparator) . splitPath

namesMatchingSplit :: FilePath -> [String] -> IO [FilePath]
namesMatchingSplit dir [] = return [dir]
namesMatchingSplit dir (pat:patParts)
  | isExtPattern pat = namesMatchingExt dir pat
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
    pathAndSubpaths path = unsafeInterleaveIO $ do
      subpaths <- namesMatchingSplit path patParts
      return subpaths

isHidden ('.':_) = True
isHidden _ = False

listMatches dir filePat = do
  files <- catchIOError
           (getDirectoryContents dir)
           (const $ return []) 
  return
    $ map (dir </>)
    $ sortOn (dropWhile (`elem` ".#*") . map toLower)
    $ filter hidden
    $ filter (`matches` filePat) files
  where hidden
          | isHidden filePat = isHidden
          | otherwise = not . isHidden

matches name pat
  | pathSeparator == '/' = matchesGlob False name pat
  | otherwise = matchesGlob True name pat

listPlain dir file = do
  let path = dir </> file
  e <- doesPathExist path
  if (e)
    then return [path]
    else return []

listAll :: FilePath -> IO [FilePath]
listAll dir = do
  files <- catchIOError
           (listDirectory dir)
           (const $ return [])
  files' <- filterM ((not <$>) . isCyclicLink dir) files
  sublists <- unsafeInterleaveIO $ mapM (listAll . (dir </>)) files'
  return $ dir:concat sublists

isCyclicLink dir file = do
  let path = dir </> file
  symlink <- pathIsSymbolicLink path
  if (symlink)
    then do
      absoluteDirPath <- makeAbsolute dir
      targetPath <- readSymbolicLink path
      absolutTargetPath <- makeAbsolute targetPath
      return $ absolutTargetPath `isPrefixOf` absolutTargetPath
    else return False

namesMatchingExt :: FilePath -> String -> IO [FilePath]
namesMatchingExt dir pat = do
  files <- listAll dir
  return $ filter (`matches` pat) files
