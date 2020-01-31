{-# LANGUAGE FlexibleContexts #-}
module HttpRequestParser where

import LimitedStream (limit)
import Numeric (readHex)
import Text.Parsec.Prim (ParsecT, Stream, parserZero, try)
import Text.ParserCombinators.Parsec hiding (token, try)
import Text.Printf (printf)
import URIParser (URI, absoluteUri, absPath, authority)

-- Helper functions and parsers
linearSpace :: Stream s m Char => ParsecT s u m ()
linearSpace = () <$ many (oneOf " \t")

spaced :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
spaced p = linearSpace *> p <* linearSpace

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

-- https://tools.ietf.org/html/rfc2616#section-2.1
list1 :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
list1 p = do
  v1 <- many lws *> p
  vs <- many (try (many lws *> char ',' *> many lws *> p))
  return $ v1:vs

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
  l0 <- try (True <$ many1 lws) <|> return False
  if l0
    then startLws
    else startText
  where exceptCharP = satisfy (not . (`elem` exceptChars))
        startLws    = do
          mt <- try (Just <$> many1 exceptCharP) <|> return Nothing
          case mt of
            Just t  -> (" " ++) . (t ++) <$> cont
            Nothing -> return " "
        startText   = (++) <$> many1 exceptCharP <*> cont
        cont        = do
          l <- try (True <$ many1 lws) <|> return False
          if l
            then (" " ++) <$> cont
            else return ""

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

-- https://tools.ietf.org/html/rfc2616#section-3.1
httpVersion :: Stream s m Char => ParsecT s u m (Int, Int)
httpVersion = string "HTTP/" *> v
  where v = do
          d1 <- many1 digit
          char '.'
          d2 <- many1 digit
          return (read d1, read d2)

-- https://tools.ietf.org/html/rfc2616#section-3.2
-- See https://tools.ietf.org/html/rfc2396#section-3

-- https://tools.ietf.org/html/rfc2616#section-3.6
data TransferCoding =
    Chunked
  | TransferExtension String
  deriving Show

transferCoding :: Stream s m Char => ParsecT s u m TransferCoding
transferCoding =
      try (Chunked <$ string "chunked")
  <|> TransferExtension <$> transferExtension

transferExtension :: Stream s m Char => ParsecT s u m String
transferExtension = do
  t  <- token
  ps <- many ((:) <$> char ';' <*> parameter)
  return $ t ++ concat ps

parameter :: Stream s m Char => ParsecT s u m String
parameter = do
  a <- attribute
  s <- string "="
  v <- value
  return $ a ++ s ++ v

attribute :: Stream s m Char => ParsecT s u m String
attribute = token
value     :: Stream s m Char => ParsecT s u m String
value     = token <|> quotedString

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
trailer = many (entityHeader <* crlf)

-- https://tools.ietf.org/html/rfc2616#section-4.2
messageHeader :: Stream s m Char => ParsecT s u m Header
messageHeader = do
  n <- fieldName
  char ':'
  v <- optionMaybe fieldValue
  return $ ExtensionHeader n v

fieldName    :: Stream s m Char => ParsecT s u m String
fieldName    = token
fieldValue   :: Stream s m Char => ParsecT s u m String
fieldValue   = concat
  <$> many (try fieldContent <|> (" " <$ try lws))
fieldContent :: Stream s m Char => ParsecT s u m String
fieldContent = spaced text

-- https://tools.ietf.org/html/rfc2616#section-4.3
-- TODO support chunked transfer encoding
messageBody :: Stream s m Char => Headers -> ParsecT s u m String
messageBody hs = maybe (entityBody 0) entityBody $ getContentLength hs

-- https://tools.ietf.org/html/rfc2616#section-4.5
-- TODO: Add other general headers
generalHeader :: Stream s m Char => ParsecT s u m Header
generalHeader = transferEncoding

-- https://tools.ietf.org/html/rfc2616#section-5
data HttpRequest = HttpRequest {
  hrMethod  :: HttpMethod,
  hrUri     :: RequestURI,
  hrVersion :: (Int, Int),
  hrHeaders :: Headers,
  hrBody    :: String
  } deriving Show

request :: Stream s m Char => ParsecT s u m HttpRequest
request = do
  r  <- requestLine
  hs <- many
        $ (try generalHeader <|> try requestHeader <|> entityHeader)
          <* crlf
  crlf
  b  <- messageBody hs
  eof
  return $ r hs b

-- https://tools.ietf.org/html/rfc2616#section-5.1
requestLine :: Stream s m Char =>
  ParsecT s u m (Headers -> String -> HttpRequest)
requestLine = do
  m <- method
  sp
  u <- requestUri
  sp
  v <- httpVersion
  crlf
  return $ HttpRequest m u v

-- https://tools.ietf.org/html/rfc2616#section-5.3
requestHeader = parserZero

-- https://tools.ietf.org/html/rfc2616#section-5.1.1
data HttpMethod = GET | POST | ExtensionMethod String deriving (Eq, Show)

-- TODO: Other methods
method :: Stream s m Char => ParsecT s u m HttpMethod
method =
      GET <$ string "GET"
  <|> POST <$ string "POST"
  <|> ExtensionMethod <$> token

-- https://tools.ietf.org/html/rfc2616#section-5.1.2
data RequestURI =
    NoResourceURI
  | AbsoluteRequestURI  URI
  | AbsPathRequestURI   String
  | AuthorityRequestURI String
  deriving Show

requestUri :: Stream s m Char => ParsecT s u m RequestURI
requestUri = choice [
  NoResourceURI       <$ string "*",
  AbsoluteRequestURI  <$> absoluteUri,
  AbsPathRequestURI   <$> absPath,
  AuthorityRequestURI <$> authority]

data Header =
    -- General headres
    TransferEncoding [TransferCoding]
    -- Entity headers
  | ContentLength Int
  | ExtensionHeader String (Maybe String)
  deriving Show
type Headers = [Header]

-- https://tools.ietf.org/html/rfc2616#section-7.1
-- TODO: Add other entity headers
entityHeader :: Stream s m Char => ParsecT s u m Header
entityHeader =
      try contentLength
  <|> extensionHeader

extensionHeader :: Stream s m Char => ParsecT s u m Header
extensionHeader = messageHeader

-- https://tools.ietf.org/html/rfc2616#section-7.2
entityBody :: Stream s m Char => Int -> ParsecT s u m String
entityBody l =
      count l anyChar
  <?> printf "%i characters in HTTP body" l

-- https://tools.ietf.org/html/rfc2616#section-14.13
contentLength :: Stream s m Char => ParsecT s u m Header
contentLength = do
  string "Content-Length" *> char ':'
  ds <- spaced $ many1 digit
  return . ContentLength . read $ ds

getContentLength [] = Nothing
getContentLength (ContentLength l:_) = Just l
getContentLength (_:t) = getContentLength t

-- https://tools.ietf.org/html/rfc2616#section-14.41
transferEncodingName = "Transfer-Encoding"
transferEncoding :: Stream s m Char => ParsecT s u m Header
transferEncoding = do
  string transferEncodingName *> char ':'
  c <- list1 transferCoding
  return $ TransferEncoding c
