module GlobRegex(
  globToRegex, matchesGlob,
  makeGlobRegex, GlobError(..)) where

import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Logger
import Text.Regex.Posix

data GlobError =   GlobUnterminatedClass
                 | GlobUnterminatedNegativeClass
                 | GlobEmptyClass deriving (Eq, Show)

type GlobRegex  = Logger (Either GlobError String)
type GlobRegexT = ExceptT GlobError Logger String

globToRegex :: String -> GlobRegex
globToRegex = runExceptT . globToRegexT

globToRegexT :: String -> GlobRegexT
globToRegexT g = do
  regex <- globToRegex' g
  return $ '^':regex ++ "$"

addLogT :: String -> ExceptT GlobError Logger ()
addLogT =  ExceptT . fmap Right . addLog

globToRegex' :: String -> GlobRegexT
globToRegex' "" = return ""
globToRegex' ('[':str) =
  globClassOpen str >>= return . ('[':)
globToRegex' ('*':str) =
  addLogT "Any" >>
  globToRegex' str >>= return . (".*" ++)
globToRegex' ('?':str) =
  addLogT "Single char" >>
  globToRegex' str >>= return . (".?" ++ )
globToRegex' (c:str) = do
  c' <- escapeT c
  globToRegex' str >>= return . (c' ++)

escapeT :: Char -> ExceptT GlobError Logger String
escapeT = ExceptT . fmap Right . escape

escape :: Char -> Logger String
escape c
  | escapeChar c = addLog ("Escape " ++ [c]) >> (return $ '\\':[c])
  | otherwise = return [c]
  where escapeChar = (`elem` "\\.+()^${}]|")

globClassOpen :: String -> GlobRegexT
globClassOpen "" = ExceptT . return . Left $ GlobUnterminatedClass
globClassOpen (']':_) = ExceptT . return . Left $ GlobEmptyClass
globClassOpen ('!':'!':']':str) = do
  addLogT "Negative character class"
  r <- globToRegex' str
  return $ '^':'!':']':r
globClassOpen ('!':']':str) = do
  -- Character class of a single '!'
  addLogT "Character class"
  r <- globToRegex' str
  return $ '!':']':r
globClassOpen "!" = ExceptT . return . Left $ GlobUnterminatedNegativeClass
globClassOpen ('!':str) =
  addLogT "Negative character class" >>
  globClass True str >>= return . ('^':)
globClassOpen str = addLogT "Character class" >> globClass False str

globClass :: Bool -> String -> GlobRegexT
globClass True  "" = ExceptT . return . Left $ GlobUnterminatedNegativeClass
globClass False "" = ExceptT . return . Left $ GlobUnterminatedClass
globClass _ (']':str) =
  globToRegex' str >>= return . (']':)
globClass n (c:str) = do
  c' <- escapeT c
  globClass n str >>= return . (c' ++)

makeGlobRegex b = runExceptT . makeGlobRegexT b

makeGlobRegexT False pat = do
  regex <- globToRegexT pat
  return $ makeRegex regex
makeGlobRegexT True pat = do
  regex <- globToRegexT pat
  return
    $ makeRegexOpts (compIgnoreCase + defaultCompOpt)
      defaultExecOpt regex

matchesGlob b n = runExceptT . matchesGlobT b n

matchesGlobT :: Bool -> FilePath -> String ->
  ExceptT GlobError Logger Bool
matchesGlobT caseInsensitive name pat = do
  regex <- makeGlobRegexT caseInsensitive pat
  return $ match regex name
