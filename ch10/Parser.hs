module Parser(assert, byte, bytes,
              byteString,
              char, chars, getState, parse,
              parseIO, parseS, peek,
              nat, offset, string,
              setState, take,
              takeWhile, takeWhileNotSpace,
              takeWhileSpace,
              Parse) where

import           Control.Monad.Trans.Except (ExceptT(..))
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char (chr, isDigit, isSpace)
import           Data.Word
import           Prelude hiding (take, takeWhile)

data ParseState = ParseState
  { offset :: Int, string :: L.ByteString }

instance Show ParseState where
  show s = "ParseState offset=" ++ (show $ offset s) ++", string=" ++ str
    where str     = show $ h ++ t
          (h, t)  = fmap dots $ splitAt 10 $ L8.unpack $ string s
          dots "" = ""
          dots _  = "..."

newtype Parse a = Parse {
    runParse :: ParseState -> Either String (a, ParseState)
  }

parse   p s = runParse p $ ParseState 0 s
parseS  p s = parse p $ L8.pack s
parseIO :: Parse a -> IO L.ByteString -> ExceptT String IO (a, ParseState)
parseIO p s = ExceptT $ parse p <$> s

getState   = Parse $ \s -> Right (s, s)
setState s = Parse $ \_ -> Right (s, s)

instance Functor Parse where
  f `fmap` p = do
    v <- p
    return $ f v

instance Applicative Parse where
  pure a = return a
  pf <*> pa = do
    f <- pf
    a <- pa
    return $ f a

instance Monad Parse where
  pa >>= f = Parse $ \s ->
    do
      (a, s') <- runParse pa s
      let pb = f a
      runParse pb s'
  return a = Parse $ \s -> Right (a, s)
  fail msg = Parse $ \s ->
    Left $ "Error at offset " ++ (show $ offset s) ++ ": " ++ msg

assert :: String -> Bool -> Parse ()
assert _ True  = return ()
assert m False = fail m

byte :: Parse Word8
byte = do
  s <- getState
  case L.uncons $ string s of
    Nothing -> fail "Empty string"
    Just (h, t) -> do
      setState s {
        offset = (offset s) + 1,
        string = t
        }
      return h
      
char :: Parse Char
char = chr . fromIntegral <$> byte

peek :: Parse a -> Parse a
peek p = do
  s <- getState
  a <- p
  setState s
  return a

peekMaybe :: Parse a -> Parse (Maybe a)
peekMaybe p = Parse $ \s ->
  case runParse p s of
    Left _ -> Right (Nothing, s)
    Right (a, _) -> Right (Just a, s)

takeWhile :: (a -> Bool) -> Parse a -> Parse [a]
takeWhile f p = do
  ma <- peekMaybe p
  case ma of
    Nothing -> return []
    Just a ->
      if f a
      then
        do
          a' <- p
          t  <- takeWhile f p
          return (a':t)
      else return []

take :: Int -> Parse a -> Parse [a]
take 0 _ = return []
take n p = do
  a <- p
  t <- take (n - 1) p
  return $ a:t

-- null p = maybe True (const False) <$> peekMaybe p

bytes = (`take` byte)
chars = (`take` char)

byteString n = do
  s <- getState
  setState $ s {
    offset = offset s + n,
    string = L.drop n64 $ string s
    }
  return $ L.take n64 $ string s
  where n64 = fromIntegral n

parseInt :: Parse Int
parseInt = do
  c <- peek char
  if c == '-'
    then do
      char
      parse (-1)
    else parse 1
  where parse sign = do
          s <- takeWhile isDigit char
          if null s
            then fail "No digits"
            else return $ sign * (read s)

nat :: Parse Int
nat = do
  i <- parseInt
  if (i >= 0)
    then return i
    else fail "Negative number"

takeWhileNotSpace = takeWhile (not . isSpace) char
takeWhileSpace    = takeWhile isSpace char
