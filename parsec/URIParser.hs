{-# LANGUAGE FlexibleContexts #-}
module URIParser where

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
type Authority = String
type AbsPath   = String
type Scheme    = String
type Query     = String

data URI =
    HierarchicalNetURI Scheme Authority (Maybe AbsPath) (Maybe Query)
  | HierarchicalAbsURI Scheme AbsPath (Maybe Query)
  | OpaqueURI Scheme String
  deriving (Eq, Show)

uriScheme (HierarchicalNetURI s _ _ _) = s
uriScheme (HierarchicalAbsURI s _   _) = s
uriScheme (OpaqueURI          s _)     = s

uriAuthority (HierarchicalNetURI _ a _ _) = Just a
uriAuthority _                            = Nothing

uriPath (HierarchicalNetURI _ _ p _) =      p
uriPath (HierarchicalAbsURI _   p _) = Just p
uriPath _                            = Nothing

uriQuery (HierarchicalNetURI _ _ _ q) = q
uriQuery (HierarchicalAbsURI _   _ q) = q
uriQuery _                            = Nothing

absoluteUri :: Stream s m Char => ParsecT s u m URI
absoluteUri = do
  s <- scheme
  char ':'
  p <- hierPart <|> opaquePart
  return $ p s

hierPart, opaquePart :: Stream s m Char => ParsecT s u m (Scheme -> URI)
hierPart = (try netPath' <|> absPath') <*> qquery
  where qquery   = optionMaybe $ char '?' *> query
        netPath' = do
          (a, p) <- netPath
          return $ \q s -> HierarchicalNetURI s a p q
        absPath' = do
          p <- absPath
          return $ \q s -> HierarchicalAbsURI s p q

netPath :: Stream s m Char => ParsecT s u m (String, Maybe AbsPath)
netPath = do
  string "//"
  a <- authority
  p <- optionMaybe absPath
  return (a, p)
absPath :: Stream s m Char => ParsecT s u m AbsPath
absPath = (:) <$> char '/' <*> pathSegments

opaquePart  = do
  p <- ((:) <$> uricNoSlash <*> many uric)
  return $ \s -> OpaqueURI s p
uricNoSlash :: Stream s m Char => ParsecT s u m Char
uricNoSlash = unreserved <|> escaped <|> oneOf ";?:@&=+$,"

-- https://tools.ietf.org/html/rfc2396#section-3.1
scheme :: Stream s m Char => ParsecT s u m String
scheme = (:) <$> alpha <*> many (alpha <|> digit <|> oneOf "+-.")

-- https://tools.ietf.org/html/rfc2396#section-3.2
authority :: Stream s m Char => ParsecT s u m Authority
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
