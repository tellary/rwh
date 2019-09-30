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
    start patWithDir = namesMatchingSplit patHead patTail
      where
        (patHead:patTail) =
          map dropTrailingPathSeparator
          $ splitPath patWithDir

namesMatchingSplit :: FilePath -> [String] -> IO NamesMatching
namesMatchingSplit dir [] = return $ Right [dir]
namesMatchingSplit dir (pat:patParts) = do
  pathsEither <- listDir dir pat
  case pathsEither of
    Right paths -> subpaths paths patParts
    l -> return $ l

listDir :: FilePath -> String -> IO NamesMatching
listDir dir pat
  | isExtPattern pat = namesMatchingExt dir pat
  | isPattern pat = listMatches dir pat
  | otherwise = Right <$> listPlain dir pat

subpaths :: [FilePath] -> [String] -> IO NamesMatching
subpaths paths patParts =
  fmap concat
  -- If first result of `namesMatchingSplit` is `Right`,
  -- then we could assume subsequent results will be also `Right`,
  -- because pattern compiles.
  -- This is workaround for `sequence` of `Either` being eager.
  . sequenceFailFirst
  <$> lazyMapM (`namesMatchingSplit` patParts) paths

isHidden ('.':_) = True
isHidden _ = False

-- Little type hint for GHC to use in listMatches
matchRegex :: Regex -> String -> Bool
matchRegex = match

listMatches dir filePat = withPattern doListMatches filePat
  where doListMatches regex = do
          files <- catchIOError
                   (getDirectoryContents dir)
                   (const $ return [])
          return
            $ map (dir </>)
            $ sortOn (dropWhile (`elem` ".#*") . map toLower)
            $ filter hidden
            $ filter (matchRegex regex) files
        hidden
          | isHidden filePat = isHidden
          | otherwise = not . isHidden

listPlain dir file = do
  let path = dir </> file
  e <- doesPathExist path
  if (e)
    then return [path]
    else return []

listAll :: FilePath -> IO [FilePath]
listAll = fmap tail . (flip listAllSubpaths "")

listAllSubpaths :: FilePath -> FilePath -> IO [FilePath]
listAllSubpaths dir subpath = do
  files <- catchIOError
           (listDirectory currentDir)
           (const $ return [])
  files' <- filterM ((not <$>) . isCyclicLink currentDir) files
  sublists <- unsafeInterleaveIO
              $ mapM (listAllSubpaths dir . (subpath </>)) files'
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

makeGlobRegexSep pat
  | pathSeparator == '/' = makeGlobRegex False pat
  | otherwise = makeGlobRegex True pat

withPattern :: (Regex -> IO b) -> String -> IO (Either GlobError b)
withPattern f pat =
  case makeGlobRegexSep pat of
    Right regex -> Right <$> f regex
    Left err -> return $ Left err

namesMatchingExt :: FilePath -> String -> IO NamesMatching
namesMatchingExt dir pat =
  withPattern
  (\r -> listAll dir >>= return . filter (match r))
  (dir </> pat)

unsafeRight (Right x) = x
unsafeRight _ = error "Left is unexpected"

sequenceFailFirst ((Right x):xs) = Right $ x:map unsafeRight xs
sequenceFailFirst (Left e:_) = Left e
sequenceFailFirst [] = Right []

-- Dirty hack from here:
-- https://stackoverflow.com/a/12609802/1060693
-- It's necessary to make the following work in reasonable time:
-- fmap (take 2) <$> namesMatching "/h**/.ssh/*"
lazyMapM :: (a -> IO b) -> [a] -> IO [b]
lazyMapM _ [] = return []
lazyMapM f (x:xs) = do y <- f x
                       ys <- unsafeInterleaveIO $ lazyMapM f xs
                       return (y:ys)
