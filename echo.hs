import Data.Char
import System.IO

mySetBuffering mode = do
  hSetBuffering stdin mode
  hSetBuffering stdout mode

inputLoop = do
  str <- getContents
  mapM_ putChar $ takeWhile ((/= 27) . ord) str

main = do
  -- mySetBuffering (BlockBuffering $ Just 12)
  -- This doesn't work (does LineBuffering)
  -- The following may be related:
  -- https://gitlab.haskell.org/ghc/ghc/issues/2189
  mySetBuffering NoBuffering
  inputLoop
  
