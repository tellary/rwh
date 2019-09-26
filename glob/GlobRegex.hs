module GlobRegex(globToRegex, matchesGlob, GlobError) where

import Text.Regex.Posix

data GlobError = GlobUnterminatedClass |
                 GlobEmptyClass deriving Show

type GlobRegex = Either GlobError String

globToRegex :: String -> GlobRegex
globToRegex g = do
  regex <- globToRegex' g
  return $ '^':regex ++ "$"

globToRegex' :: String -> GlobRegex
globToRegex' "" = Right ""
globToRegex' ('[':str) =
  globClassOpen str >>= return . ('[':)
globToRegex' ('*':str) =
  globToRegex' str >>= return . (".*" ++)
globToRegex' ('?':str) =
  globToRegex' str >>= return . (".?" ++ )
globToRegex' (c:str) =
  globToRegex' str >>= return . ((escape c) ++)

escape c
  | escapeChar c = '\\':[c]
  | otherwise = [c]
  where escapeChar = (`elem` "\\.+()^${}]|")

globClassOpen :: String -> GlobRegex
globClassOpen "" = Left GlobUnterminatedClass
globClassOpen (']':_) = Left GlobEmptyClass
globClassOpen ('!':str) = globClassOpenN ('!':str)
globClassOpen str = globClass str
-- matchesGlob False "f!!.c" "f[!][!].c"
-- matchesGlob False "fa.c"  "f[!!].c"
globClassOpenN ('!':']':str) = do
  r <- globToRegex' str
  return $ '!':']':r
globClassOpenN "!" = globClassOpen ""
globClassOpenN ('!':str) =
  globClass str >>= return . ('^':)
globClass "" = globClassOpen ""
globClass (']':str) =
  globToRegex' str >>= return . (']':)
globClass (c:str) =
  globClass str >>= return . ((escape c) ++)

matchesGlob :: Bool -> FilePath -> String -> Either GlobError Bool
matchesGlob False name pat = do
  regex <- globToRegex pat
  return $ name =~ regex
matchesGlob True  name pat = do
  regex <- globToRegex pat
  return
    $ match
      (makeRegexOpts
         (compIgnoreCase + defaultCompOpt)
         defaultExecOpt
         regex)
      name

bad = globToRegex "fo[.c"

-- matchesGlob False "fo].c" "fo].c"
