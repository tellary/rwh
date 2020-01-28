{-# LANGUAGE FlexibleContexts #-}
module URI where

import Data.List (intercalate)
import Numeric (readHex)
import Text.Parsec.Prim (ParsecT, Stream, try)
import Text.ParserCombinators.Parsec hiding (try)

-- https://tools.ietf.org/html/rfc2396#section-1.6
alpha, lowalpha, upalpha :: Stream s m Char => ParsecT s u m Char
alpha    = lowalpha <|> upalpha
lowalpha = oneOf ['a'..'z']
upalpha  = oneOf ['A'..'Z']

-- https://tools.ietf.org/html/rfc2396#section-2
uric, reserved, escaped, unreserved, mark ::
  Stream s m Char => ParsecT s u m Char
uric = reserved <|> unreserved <|> escaped

-- https://tools.ietf.org/html/rfc2396#section-2.2
reserved = oneOf ";/?:@&=+$|,"

-- https://tools.ietf.org/html/rfc2396#section-2.4
escaped = char '%' *>
  (toEnum . fst . head . readHex <$> count 2 hexDigit)

-- https://tools.ietf.org/html/rfc2396#section-2.3
unreserved = alphaNum <|> mark
mark = oneOf "-_.!~*'()"

-- https://tools.ietf.org/html/rfc2396#section-3
absoluteUri, hierPart, opaquePart, netPath, absPath ::
  Stream s m Char => ParsecT s u m String
absoluteUri = do
  s <- scheme
  char ':'
  p <- hierPart <|> opaquePart
  return $ s ++ [':'] ++ p

hierPart = (++) <$> (netPath <|> absPath) <*> qquery
  where qquery = option "" $ (:) <$> char '?' <*> query

netPath = do
  string "//"
  a <- authority
  p <- option "" absPath
  return $ "//" ++ a ++ p

absPath = (:) <$> char '/' <*> pathSegments

opaquePart  = (:) <$> uricNoSlash <*> many uric
uricNoSlash :: Stream s m Char => ParsecT s u m Char
uricNoSlash = unreserved <|> escaped <|> oneOf ";?:@&=+$,"

-- https://tools.ietf.org/html/rfc2396#section-3.1
scheme :: Stream s m Char => ParsecT s u m String
scheme = (:) <$> alpha <*> many (alpha <|> digit <|> oneOf "+-.")

-- https://tools.ietf.org/html/rfc2396#section-3.2
authority :: Stream s m Char => ParsecT s u m String
authority = server <|> regName

-- https://tools.ietf.org/html/rfc2396#section-3.2.1
regName :: Stream s m Char => ParsecT s u m String
regName = many1 $ unreserved <|> escaped <|> oneOf "$,;:@&=+"

-- https://tools.ietf.org/html/rfc2396#section-3.2.2
server, userinfo, hostport, host, hostname,
  domainlabel, toplevel, ipv4Address, port
  :: Stream s m Char => ParsecT s u m String
server =
      (try $ (++) <$> userinfo' <*> hostport)
  <|> (try $ hostport)
  <|> return ""
  where userinfo' = do
          u <- userinfo
          char '@'
          return $ u ++ ['@']
userinfo = many $ unreserved <|> escaped <|> oneOf ";:&=+$,"
hostport = (++) <$> host <*> option "" ((:) <$> char ':' <*> port)
host     = hostname <|> ipv4Address
hostname =
  (try $ do
      l <- domainlabel
      char '.'
      h <- hostname
      return $ l ++ "." ++ h)
  <|> (++) <$> toplevel <*> option "" (string ".")

domainlabel = do
  a1   <- many1 alphaNum
  cont <- option "" $ (++) <$> many1 (char '-') <*> domainlabel
  return $ a1 ++ cont
toplevel    = do
  a1  <- many1 alpha
  cont <- option "" $ (++) <$> many1 (char '-') <*> domainlabel
  return $ a1 ++ cont
ipv4Address = do
  ds <- count 3 (many1 digit <* char '.')
  d4 <- many1 digit
  return $ intercalate "." $ ds ++ [d4]

port = many1 digit

-- https://tools.ietf.org/html/rfc2396#section-3.3
pathSegments, segment, param :: Stream s m Char => ParsecT s u m String
pathSegments = do
  s1 <- segment
  ss <- many (char '/' *> segment)
  return $ intercalate "/" $ s1:ss
segment = do
  p1 <- many pchar
  ps <- many (char ';' *> param)
  return $ intercalate ";" $ p1:ps
param = many pchar
pchar :: Stream s m Char => ParsecT s u m Char
pchar = unreserved <|> escaped <|> oneOf ":@&=+$,"

-- https://tools.ietf.org/html/rfc2396#section-3.4
query :: Stream s m Char => ParsecT s u m String
query = many uric
