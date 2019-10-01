-- (`matchGlob` "abc") <$> makeGlob "*[!d]"

module GlobMatcher(Glob, matchGlob, makeGlob) where

data GlobError = GlobUnterminatedClass |
                 GlobEmptyClass deriving Show

data GlobElement = Any |
                   SingleChar |
                   Single Char |
                   Class [Char] |
                   NotClass [Char]
                   deriving Show

type Glob = [GlobElement]

matchGlob :: Glob -> String -> Bool
matchGlob [] []    = True
matchGlob [Any] [_] = True
matchGlob _  [] = False
matchGlob [] _  = False
matchGlob g@(Any:gt) s@(_:t)
  | matchGlob g t = True
  | otherwise     = matchGlob gt s
matchGlob ((Single gc):gt) (c:t)
  | gc == c = matchGlob gt t
  | otherwise = False
matchGlob ((Class gcs):gt) (c:t)
  | any (c ==) gcs = matchGlob gt t
  | otherwise = False
matchGlob ((NotClass gcs):gt) (c:t)
  | any (c /=) gcs = matchGlob gt t
  | otherwise = False  
matchGlob (SingleChar:gt) (_:t) = matchGlob gt t

makeGlob :: String -> Either GlobError Glob
makeGlob [] = Right []
makeGlob ('*':t) = (Any:) <$> makeGlob t
makeGlob ('?':t) = (SingleChar:) <$> makeGlob t
makeGlob ('[':'!':t) = makeGlobClass True [] t
makeGlob ('[':t) = makeGlobClass False [] t
makeGlob (c:t) = ((Single c):) <$> makeGlob t

makeGlobClass _ _ [] = Left GlobUnterminatedClass
makeGlobClass _ [] (']':_) = Left GlobEmptyClass
makeGlobClass False chars (']':t)  = (Class chars:) <$> makeGlob t
makeGlobClass True chars (']':t)  = (NotClass chars:) <$> makeGlob t
makeGlobClass neg chars (c:t) = makeGlobClass neg (c:chars) t

