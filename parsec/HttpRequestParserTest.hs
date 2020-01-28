import Control.Exception (assert)
import Data.Either (fromLeft, isLeft)
import HttpRequestParser
import LimitedStream
import Text.ParserCombinators.Parsec

r1 = parseLimitedFromFile httpRequestP "1.http"
r2 = parseLimitedFromFile httpRequestP "2.http"

eitherError = either (error "Not Right")
fromRightOrError = eitherError id

t1 = assert . (== GET) . eitherError method <$> r1 <*> pure "r1 is GET"
t2 = assert . (== GET) . eitherError method <$> r2 <*> pure "r2 is GET"

t3 = assert . (== "/test")
  .   eitherError uri     <$> r1 <*> pure "r1 uri is /test"
t4 = assert . (== "https://google.com")
  .   eitherError uri     <$> r2 <*> pure "r1 uri is https://google.com"
t5 = assert . (== [("q", Just "test")])
  .   eitherError params  <$> r2 <*> pure "r2 params are q=test"
t6 = assert . (== "1234567890123456789012345")
  .   eitherError body    <$> r1 <*> pure "r1 body is 25 chars"
t7 = assert . (== [("Content-Length", "25")])
  .   eitherError headers <$> r1
  <*> pure "r1 headers is [(Content-Length: 25)]"

t8 = assert
     (isLeft . parse (string "abc") "" . LimitedStream 2 $ cycle "abc")
     "Parsing \"abc\" on a stream limited to 2 fails"
t9 = assert
     (3 == (sourceColumn . errorPos . fromLeft undefined
      . parse (limit 2 (char 'a' *> char 'b' *> char 'c')) ""
      $ UnlimitedStream "abc"))
     "Parsing 'a' *> 'b' *> 'c' on a stream limited to 2 fails on col 3"

t10 = assertLineCol
      <$> errorPos . fromLeft undefined
      .   parse httpRequestP "" . UnlimitedStream
      .   (++ cycle "abc") <$> readFile "2.http"
      where assertLineCol pos =
              assert (sourceColumn pos == 4097 && sourceLine pos == 2)
                     "Parsing an \"exploded\" header fails where expected"

cb1 = fromRightOrError
  $ parse chunkedBody  "" "3;f\r\n123\r\n0\r\na: b\r\n\r\n"
t11 = assert (
     [("a", "b")]                == bodyTrailer   cb1
  && [([("f", Nothing)], "123")] == bodyChunks    cb1
  && []                          == bodyLastChunk cb1)
  "Chunked body 1 is correct"
tests = sequence
  [t1, t2, t3, t4, t5,
   t6, t7, return t8, return t9, t10,
   return t11]
  >>= mapM_ putStrLn
