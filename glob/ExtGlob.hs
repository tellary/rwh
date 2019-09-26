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

-- TODO: Fix lazy I/O

type NamesMatching = Either GlobError [FilePath]

isPattern = any (`elem` "*?[")
isExtPattern = ("**" `isInfixOf`)

namesMatching :: String -> IO NamesMatching
namesMatching =
  namesMatchingSplit ""
  . (map dropTrailingPathSeparator) . splitPath

namesMatchingSplit :: FilePath -> [String] -> IO NamesMatching
namesMatchingSplit dir [] = return $ Right [dir]
namesMatchingSplit dir (pat:patParts)
  | isExtPattern pat = namesMatchingExt dir pat
  | otherwise = do
      pathsEither <- listDir dir pat
      case pathsEither of
        Right paths -> subpaths paths patParts
        l -> return $ l

listDir :: FilePath -> String -> IO NamesMatching
listDir dir pat
  | isPattern pat = listMatches dir pat
  | otherwise = Right <$> listPlain dir pat

subpaths :: [FilePath] -> [String] -> IO NamesMatching
subpaths paths patParts =
  fmap concat . sequence
  <$> mapM (unsafeInterleaveIO . (`namesMatchingSplit` patParts)) paths

isHidden ('.':_) = True
isHidden _ = False

listMatches dir filePat = do
  files <- catchIOError
           (getDirectoryContents dir)
           (const $ return []) 
  return
     $ map (dir </>)
    <$> sortOn (dropWhile (`elem` ".#*") . map toLower)
    <$> filter hidden
    <$> filterM (`matches` filePat) files
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

namesMatchingExt :: FilePath -> String -> IO NamesMatching
namesMatchingExt dir pat = do
  files <- listAll dir
  return $ filterM (`matches` pat) files
