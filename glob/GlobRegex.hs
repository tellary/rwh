module GlobRegex(globToRegex) where

globToRegex g = '^':globToRegex' g ++ "$"

globToRegex' "" = ""
globToRegex' (c:str)
  | c == '[' = '[':globClassOpen str
  | c == ']' = error "Unexpected class closing"
  | c == '*' = ".*" ++ globToRegex' str
  | c == '?' = ".?" ++ globToRegex' str
  | otherwise = (escape c) ++ globToRegex' str

escape c
  | escapeChar c = '\\':[c]
  | otherwise = [c]
  where escapeChar = (`elem` "\\\n\r.*?!")

globClassOpen "" = error "Unterminated glob class"
globClassOpen (']':_) = error "Empty class"
globClassOpen ('!':str) = '^':globClassOpenN str
globClassOpen str = globClass str
globClassOpenN ('!':str) = globClass ('!':str)
globClassOpenN str = globClassOpen str
globClass "" = globClassOpen ""
globClass (']':str) = ']':globToRegex' str
globClass (c:str) = (escape c) ++ globClass str
