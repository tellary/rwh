module GlobRegex(globToRegex, matchesGlob) where

import Text.Regex.Posix

globToRegex g = '^':globToRegex' g ++ "$"

globToRegex' "" = ""
globToRegex' ('[':str) = '[':globClassOpen str
globToRegex' ('*':str) = ".*" ++ globToRegex' str
globToRegex' ('?':str) = ".?" ++ globToRegex' str
globToRegex' (c:str) = (escape c) ++ globToRegex' str

escape c
  | escapeChar c = '\\':[c]
  | otherwise = [c]
  where escapeChar = (`elem` "\\.+()^${}]|")

globClassOpen "" = error "Unterminated character class"
globClassOpen (']':_) = error "Empty class"
globClassOpen ('!':str) = globClassOpenN ('!':str)
globClassOpen str = globClass str
-- matchesGlob "f!!.c" "f[!][!].c"
-- matchesGlob "fa.c" "f[!!].c"
globClassOpenN ('!':']':str) = '!':']':globToRegex' str
globClassOpenN "!" = globClassOpen ""
globClassOpenN ('!':str) = '^':globClass str
globClass "" = globClassOpen ""
globClass (']':str) = ']':globToRegex' str
globClass (c:str) = (escape c) ++ globClass str

matchesGlob :: Bool -> FilePath -> String -> Bool
matchesGlob False name pat = name =~ globToRegex pat
matchesGlob True  name pat =
  match
  (makeRegexOpts
    (compIgnoreCase + defaultCompOpt)
    defaultExecOpt
    $ globToRegex pat)
  name

bad = globToRegex "fo[.c"

-- matchesGlob "fo].c" "fo].c"
