module HttpRequestParser where

import LimitedStream
import Text.Parsec.Prim (try)
import Text.ParserCombinators.Parsec hiding (try)
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

httpRequestP :: LimitCharParser st HttpRequest
httpRequestP = do
  halfReq  <- HttpRequest
              <$> methodP
              <*> uriP
              <*> paramsP
  string "HTTP/1."
  string "1\n" <|> string "0\n"
  hs       <- headersP
  b        <- bodyP $ lookup contentLengthHeader hs
  return $ halfReq hs b

sp    = many  (char ' ')
sp1   = many1 (char ' ')
notSp = many  (noneOf " \t\n\r?")

methodP :: LimitCharParser st HttpMethod
methodP = (GET <$ string "GET" <|> POST <$ string "POST") <* sp1
uriP = string "*" <|> absolutePathP <|> absoluteUriP
absolutePathP = (:) <$> char '/' <*> notSp
absoluteUriP = do
  s1 <- string "http"
  s2 <- option "" (string "s")
  s3 <- string "://"
  s4 <- notSp
  return $ s1 ++ s2 ++ s3 ++ s4
paramsP :: LimitCharParser st RequestParams
paramsP = option [] (char '?' *> query) <* sp1
headersP :: LimitCharParser st Headers
headersP = limit 4096 $ option [] (endBy1 headerP newline)
headerP :: LimitCharParser st Header
headerP = (,) <$> (headerNameP <* char ':') <*> (sp *> headerValueP)
headerNameP = (:) <$> headerStartCharP <*> many headerCharP
headerStartCharP =  oneOf ['a'..'z'] <|> oneOf ['A'..'Z']
headerCharP = choice [
  headerStartCharP,
  oneOf ['0'..'9'],
  oneOf "-"]
headerValueP :: LimitCharParser st String
headerValueP = (++) <$> notSp <*> headerValueContP
headerValueContP :: LimitCharParser st String
headerValueContP =
  option ""
  $ (:) <$> (try (newline *> many1 (oneOf " \t") *> pure ' '))
        <*> headerValueP
bodyP :: Maybe String -> LimitCharParser st String
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
