module NetpbmCommon(skipToNextBlock) where

import qualified Parser                     as P

skipComment = P.takeWhile ((/= '\n')) P.char

skipToNextBlock = do
  P.takeWhileSpace
  c <- P.peek P.char
  if c == '#'
    then do
      skipComment
      skipToNextBlock
    else return ()
