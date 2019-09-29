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

-- TODO: namesMatching "/h**/.ssh/*"

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
