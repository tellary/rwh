{-# LANGUAGE FlexibleContexts #-}
module HttpRequestParser where

import LimitedStream (limit)
import Numeric (readHex)
import Text.Parsec.Prim (ParsecT, Stream, parserZero, try)
import Text.ParserCombinators.Parsec hiding (token, try)
import Text.Printf (printf)
import URLQueryParser (query)

data HttpMethod    = GET | POST deriving (Eq, Show)
type RequestParam  = (String, Maybe String)
type RequestParams = [RequestParam]
type Header        = (String, String)
type Headers       = [Header]

data HttpRequest = HttpRequest {
  method  :: HttpMethod,
  uri     :: FilePath,
  params  :: RequestParams,
  headers :: Headers,
  body    :: String
  } deriving (Eq, Show)

contentLengthHeader = "Content-Length"

httpRequestP = do
  halfReq  <- HttpRequest
              <$> methodP
              <*> uriP
              <*> paramsP
  string "HTTP/1."
  string "1\n" <|> string "0\n"
  hs       <- limit 4096 $ headersP
  b        <- bodyP $ lookup contentLengthHeader hs
  return $ halfReq hs b

sp1   = many1 (char ' ')
notSp :: Stream s m Char => ParsecT s u m String
notSp = many  (noneOf " \t\n\r?")

methodP = (GET <$ string "GET" <|> POST <$ string "POST") <* sp1
uriP = string "*" <|> absolutePathP <|> absoluteUriP
absolutePathP = (:) <$> char '/' <*> notSp
absoluteUriP = do
  s1 <- string "http"
  s2 <- option "" (string "s")
  s3 <- string "://"
  s4 <- notSp
  return $ s1 ++ s2 ++ s3 ++ s4
paramsP = option [] (char '?' *> query) <* sp1
headersP :: Stream s m Char => ParsecT s u m Headers
headersP = option [] (endBy1 headerP crlf)
headerP :: Stream s m Char => ParsecT s u m Header
headerP = (,) <$> (headerNameP <* char ':') <*> (sp *> headerValueP)
headerNameP :: Stream s m Char => ParsecT s u m String
headerNameP = (:) <$> headerStartCharP <*> many headerCharP
headerStartCharP :: Stream s m Char => ParsecT s u m Char
headerStartCharP =  oneOf ['a'..'z'] <|> oneOf ['A'..'Z']
headerCharP :: Stream s m Char => ParsecT s u m Char
headerCharP = choice [
  headerStartCharP,
  oneOf ['0'..'9'],
  oneOf "-"]
headerValueP :: Stream s m Char => ParsecT s u m String
headerValueP = (++) <$> notSp <*> headerValueContP
headerValueContP :: Stream s m Char => ParsecT s u m String
headerValueContP =
  option ""
  $ (:) <$> (try (newline *> many1 (oneOf " \t") *> pure ' '))
        <*> headerValueP
bodyP Nothing       = "" <$ eof
bodyP (Just lenStr) =
  (char '\n' <?> "body to be preceded with a blank line") *>
  case reads lenStr of
    [(l, "")] ->
          count l anyChar <* eof
      <?> printf "%i characters in HTTP body" l
    _         ->
      fail
      $ printf "'%s' header is not an integer"
        contentLengthHeader

-- Helper functions and parsers
hexNumberOrFail :: (Eq a, Num a, Stream s m Char) => String -> ParsecT s u m a
hexNumberOrFail s = case readHex s of
  [(i, "")] -> return i
  _         -> fail $ printf "Can't read hex number %s" s
nonZeroHexDigit  :: Stream s m Char => ParsecT s u m Char
nonZeroHexDigit = try $ do
  d <- hexDigit
  if d == '0'
    then parserZero
    else return d
nonZeroHexNumber :: (Eq a, Num a, Stream s m Char) => ParsecT s u m a
nonZeroHexNumber =
  hexNumberOrFail =<< (:) <$> nonZeroHexDigit <*> many hexDigit

-- https://tools.ietf.org/html/rfc2616#section-2.2
ctlChars :: [Char]
ctlChars = map toEnum $ 127:[0..31]

cr    :: Stream s m Char => ParsecT s u m ()
cr    = () <$ char '\r'
lf    :: Stream s m Char => ParsecT s u m ()
lf    = () <$ char '\n'
sp    :: Stream s m Char => ParsecT s u m ()
sp    = () <$ char ' '
ht    :: Stream s m Char => ParsecT s u m ()
ht    = () <$ char '\t'
crlf  :: Stream s m Char => ParsecT s u m ()
crlf  = () <$ string "\r\n"
lws   :: Stream s m Char => ParsecT s u m Char
lws   = ' ' <$ optional crlf <* many1 (sp <|> ht)
text  :: Stream s m Char => ParsecT s u m String
text  = text0 ctlChars
token :: Stream s m Char => ParsecT s u m String
token = many1 $ satisfy $ not . (`elem` (ctlChars ++ separatorChars))

separatorChars = "()<>@,;:\\\"/[]?={} \t"

text0 :: Stream s m Char => [Char] -> ParsecT s u m String
text0 exceptChars = do
  t1 <- many1 (satisfy (not . (`elem` exceptChars)))
  t2 <- try $ do
      s  <- lws
      t3 <- text0 exceptChars
      return $ s:t3
    <|> return ""
  return $ t1 ++ t2

quotedString :: Stream s m Char => ParsecT s u m String
quotedString = do
  char '"'
  s <- concat <$> many (qdText <|> (:[]) <$> quotedPair)
  char '"'
  return $ ['"'] ++ s ++ ['"']
qdText :: Stream s m Char => ParsecT s u m String
qdText = text0 ('"':ctlChars)
quotedPair :: Stream s m Char => ParsecT s u m Char
quotedPair = char '\\' *> anyChar

-- https://tools.ietf.org/html/rfc2616#section-3.6.1
type ChunkExt    = [(String, Maybe String)]
type Chunk       = (ChunkExt, String)
data ChunkedBody = ChunkedBody {
  bodyTrailer   :: Headers,
  bodyChunks    :: [Chunk],
  bodyLastChunk :: ChunkExt
  }

chunkedBody :: Stream s m Char => ParsecT s u m ChunkedBody
chunkedBody = do
  cs <- many chunk
  l  <- lastChunk
  t  <- trailer
  crlf
  return $ ChunkedBody t cs l
chunk :: Stream s m Char => ParsecT s u m Chunk
chunk = do
  s <- chunkSize
  e <- chunkExtension
  crlf
  d <- chunkData s
  crlf
  return (e, d)
chunkSize :: (Eq a, Num a, Stream s m Char) => ParsecT s u m a
chunkSize = nonZeroHexNumber
chunkExtension :: Stream s m Char => ParsecT s u m ChunkExt
chunkExtension =
  many (char ';' *>
         ((,) <$> chunkExtName
              <*> optionMaybe (char '=' *> chunkExtVal)))
chunkExtName :: Stream s m Char => ParsecT s u m String
chunkExtName = token
chunkExtVal :: Stream s m Char => ParsecT s u m String
chunkExtVal = token <|> quotedString
chunkData l = try (count l anyChar) <?> printf "%i characters of data" l
lastChunk :: Stream s m Char => ParsecT s u m ChunkExt
lastChunk = char '0' *> chunkExtension <* crlf
trailer :: Stream s m Char => ParsecT s u m Headers
trailer = headersP
