{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module HttpRequestParser where

import URLQueryParser (query)
import Text.Parsec.Prim (Stream, uncons)
import Text.ParserCombinators.Parsec
import Text.Printf (printf)


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

httpRequestP :: CharParser st HttpRequest
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

data LimitedStream s = LimitedStream Int s | UnlimitedStream s
instance Monad m => Stream (LimitedStream [tok]) m tok where
  uncons (LimitedStream _ [])     = return $ Nothing
  uncons (LimitedStream 0 _ )     = return $ Nothing
  uncons (LimitedStream n (t:ts)) = return (Just (t, LimitedStream (n - 1) ts))

  uncons (UnlimitedStream [])     = return $ Nothing
  uncons (UnlimitedStream (t:ts)) = return (Just (t, UnlimitedStream ts))

limit n p = do
  input <- getInput
  case input of
    UnlimitedStream wrappedInput -> do
      setInput (LimitedStream n wrappedInput)
      p
    _                     -> p

methodP = (GET <$ string "GET" <|> POST <$ string "POST") <* sp1
uriP = string "*" <|> absolutePathP <|> absoluteUriP
absolutePathP = (:) <$> char '/' <*> notSp
absoluteUriP = do
  s1 <- string "http"
  s2 <- option "" (string "s")
  s3 <- string "://"
  s4 <- notSp
  return $ s1 ++ s2 ++ s3 ++ s4
paramsP :: CharParser st RequestParams
paramsP = option [] (char '?' *> query) <* sp1
headersP :: CharParser st Headers
headersP = option [] (endBy1 headerP newline)
headerP :: CharParser st Header
headerP = (,) <$> (headerNameP <* char ':') <*> (sp *> headerValueP)
headerNameP = (:) <$> headerStartCharP <*> many headerCharP
headerStartCharP =  oneOf ['a'..'z'] <|> oneOf ['A'..'Z']
headerCharP = choice [
  headerStartCharP,
  oneOf ['0'..'9'],
  oneOf "-"]
headerValueP = (++) <$> notSp <*> headerValueContP
headerValueContP =
  option ""
  $ (:) <$> (try (newline *> many1 (oneOf " \t") *> pure ' '))
        <*> headerValueP
bodyP :: Maybe String -> CharParser st String
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
