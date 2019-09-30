module ExtGlob(namesMatching) where

import Control.Monad(filterM)
import Data.Char(toLower)
import Data.List(isInfixOf, isPrefixOf, sortOn)
import GlobRegex
import Text.Regex.Posix(match, Regex)
import System.Directory(
  doesPathExist, getDirectoryContents, listDirectory,
  makeAbsolute, pathIsSymbolicLink)
import System.FilePath
import System.IO.Error(catchIOError)
import System.IO.Unsafe(unsafeInterleaveIO)
import System.Posix.Files(readSymbolicLink)

-- TODO: namesMatching "/h**/.ssh/id_r[sa"

type NamesMatching = Either GlobError [FilePath]

isPattern = any (`elem` "*?[")
isExtPattern = ("**" `isInfixOf`)

hasNoDirectory ('/':_) = False
hasNoDirectory ('.':'/':_) = False
hasNoDirectory ('.':'.':'/':_) = False
hasNoDirectory _ = True

namesMatching :: String -> IO NamesMatching
namesMatching pat
  | hasNoDirectory pat = start ("./" ++ pat)
  | otherwise          = start pat
  where
    start "" = return $ Right []
    start patWithDir =
      case regexPath of
        Right path -> Right <$> namesMatchingSplit dir path
        Left e -> return $ Left e
      where
        regexPath =
          sequence
          $ map (makeGlobRegexTuple . dropTrailingPathSeparator)
          $ tail $ splitPath patWithDir
        dir = head $ splitPath patWithDir

namesMatchingSplit :: FilePath -> [(Regex, String)] -> IO [FilePath]
namesMatchingSplit dir [] = return [dir]
namesMatchingSplit dir (pat:patParts) = do
  paths <- listDir dir pat
  subpaths paths patParts

listDir :: FilePath -> (Regex, String) -> IO [FilePath]
listDir dir pat
  | isExtPattern (snd pat) = namesMatchingExt dir pat
  | isPattern (snd pat) = listMatches dir pat
  | otherwise = listPlain dir $ snd pat

subpaths :: [FilePath] -> [(Regex, String)] -> IO [FilePath]
subpaths paths patParts =
  concat <$> lazyMapM (`namesMatchingSplit` patParts) paths

isHidden ('.':_) = True
isHidden _ = False

-- Little type hint for GHC to use in listMatches
matchRegex :: Regex -> String -> Bool
matchRegex = match

listMatches dir (regex, filePat) = do
  files <- catchIOError
           (getDirectoryContents dir)
           (const $ return [])
  return
    $ map (dir </>)
    $ sortOn (dropWhile (`elem` ".#*") . map toLower)
    $ filter hidden
    $ filter (matchRegex regex) files
  where
    hidden
      | isHidden filePat = isHidden
      | otherwise = not . isHidden

listPlain dir file = do
  let path = dir </> file
  e <- doesPathExist path
  if (e)
    then return [path]
    else return []

listAllRelative :: FilePath -> IO [FilePath]
listAllRelative = fmap tail . flip listAllRelativeSubpaths ""

listAllRelativeSubpaths :: FilePath -> FilePath -> IO [FilePath]
listAllRelativeSubpaths dir subpath = do
  files <- catchIOError
           (listDirectory currentDir)
           (const $ return [])
  files' <- filterM ((not <$>) . isCyclicLink currentDir) files
  sublists <- unsafeInterleaveIO
              $ mapM (listAllRelativeSubpaths dir . (subpath </>)) files'
  return $ subpath:concat sublists
  where currentDir = dir </> subpath

isCyclicLink dir file = do
  let path = dir </> file
  symlink <- pathIsSymbolicLink path
  if (symlink)
    then do
      targetPath <- readSymbolicLink path
      absolutTargetPath <- makeAbsolute targetPath
      return $ absolutTargetPath `isPrefixOf` absolutTargetPath
    else return False

makeGlobRegexTuple :: String -> Either GlobError (Regex, String)
makeGlobRegexTuple pat =
  case build of
    Right r -> Right (r, pat)
    Left l -> Left l
  where build
          | pathSeparator == '/' = makeGlobRegex False pat
          | otherwise = makeGlobRegex True pat

namesMatchingExt :: FilePath -> (Regex, String) -> IO [FilePath]
namesMatchingExt dir (r, _) =
  listAllRelative dir >>= return . map (dir </>) . filter (match r)

-- Dirty hack from here:
-- https://stackoverflow.com/a/12609802/1060693
-- It's necessary to make the following work in reasonable time:
-- fmap (take 2) <$> namesMatching "/h**/.ssh/*"
lazyMapM :: (a -> IO b) -> [a] -> IO [b]
lazyMapM _ [] = return []
lazyMapM f (x:xs) = do y <- f x
                       ys <- unsafeInterleaveIO $ lazyMapM f xs
                       return (y:ys)
