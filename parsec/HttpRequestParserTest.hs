import Control.Exception (assert)
import Data.Either (fromLeft, isLeft)
import HttpRequestParser
import LimitedStream
import Text.ParserCombinators.Parsec hiding (token, try)

r1 = fromRightOrError <$> parseLimitedFromFile request "1.http"
r2 = fromRightOrError <$> parseLimitedFromFile request "2.http"
r3 = fromRightOrError <$> parseLimitedFromFile request "3.http"
r4 = parseLimitedFromFile request "4.http"
r5 = fromRightOrError <$> parseLimitedFromFile request "5.http"
r6 = fromRightOrError <$> parseLimitedFromFile request "6.http"

tr4IsLeft = assert . isLeft <$> r4 <*> pure "r4 fails to parse"

eitherError = either (error "Not Right")
fromRightOrError = eitherError id

assertThat ::
  (a -> b) -> (b -> Bool) -> IO a -> c -> IO c
assertThat f p r m = assert . p . f <$> r <*> pure m

assertThatEq f e = assertThat f (== e)

assertManyThat f ds = sequence . map (\(p, r, m) -> assertThat f p r m) $ ds

assertManyThatEq f = assertManyThat f . map (first (==))

first f (a, b, c) = (f a, b, c)

assertMethods = assertManyThatEq hrMethod
tMethods =
  assertMethods
  [(GET, r1, "r1 method"),
   (GET, r2, "r2 method"),
   (GET, r3, "r3 method"),
   (GET, r5, "r5 method")]

assertSchemes = assertManyThatEq hrScheme
tSchemes =
  assertSchemes
  [(Nothing       , r1, "r1 scheme"),
   ((Just "https"), r2, "r2 scheme"),
   ((Just "https"), r3, "r3 scheme"),
   ((Just "http") , r5, "r5 scheme"),
   ((Just "file") , r6, "r6 scheme")]

assertAuthorities = assertManyThat hrAuthority . map (first (==))
tAuthorities =
  assertAuthorities
  [(Nothing            , r1, "r1 authority"),
   ((Just "google.com"), r2, "r2 authority"),
   ((Just "google.com"), r3, "r3 authority"),
   ((Just "fileserver"), r5, "r5 authority"),
   (Nothing            , r6, "r6 authority")]

assertPaths = assertManyThat hrPath . map (first (==))
tPaths =
  assertPaths
  [((Just "/index.html")         , r1, "r1 path"),
   (Nothing                      , r2, "r2 path"),
   (Nothing                      , r3, "r3 path"),
   ((Just "/file.txt")           , r5, "r5 path"),
   ((Just "/fileserver/file.txt"), r6, "r6 path")]

assertQueries = assertManyThat hrQuery . map (first (==))
tQueries =
  assertQueries
  [(Nothing      , r1, "r1 query"),
   (Just "q=test", r2, "r2 query"),
   (Just "q=test", r3, "r3 query"),
   (Nothing      , r5, "r5 query"),
   (Nothing      , r6, "r6 query")]

tr3ContentLength =
  assertThatEq
  (headersContentLength . hrHeaders)
  (Just 25)
  r3
  "r3 Content-Length: 25"

tr1Headers =
  assertThatEq
  hrHeaders [ContentLength 25]
  r1 "r1 headers are [ContentLength 25]"
tr6Headers =
  assertThatEq
  hrHeaders [TransferEncoding [Chunked]]
  r6 "r6 headers are [TransferEncoding [Chunked]]"

t6 =
  assertThatEq
  hrBody (EntityBody "1234567890123456789012345")
  r1 "r1 body is 25 chars"

t8 = return
  $ assert
    (isLeft . parse (string "abc") "" . LimitedStream 2 $ cycle "abc")
    "Parsing \"abc\" on a stream limited to 2 fails"

t9 = return
  $ assert
    (3 == (sourceColumn . errorPos . fromLeft undefined
           . parse (limit 2 (char 'a' *> char 'b' *> char 'c')) ""
           $ UnlimitedStream "abc"))
    "Parsing 'a' *> 'b' *> 'c' on a stream limited to 2 fails on col 3"

explodedRequest = (++ cycle "abc")
  <$> reverse <$> drop 2 <$> reverse <$> readFile "2.http"

t10 =
  assertLineCol
  <$> errorPos . fromLeft undefined
  .   parse request "" . UnlimitedStream <$> explodedRequest
  where assertLineCol pos =
          assert (sourceColumn pos == 4097 && sourceLine pos == 2)
          "Parsing an \"exploded\" header fails where expected"

chunkedBody1 = fromRightOrError
  $ parse chunkedBody  "" "3;f\r\n123\r\n0\r\na: b\r\n\r\n"

t11 = return
  $ assert (
         [ExtensionHeader "a" (Just "b")] == cbTrailer   chunkedBody1
      && [([("f", Nothing)], "123")]      == cbChunks    chunkedBody1
      && []                               == cbLastChunk chunkedBody1)
    "Chunked body 1 is correct"


tr6ChunkedBodyTrailer =
  assertThatEq
  (cbTrailer . hrBody)
  [ExtensionHeader "h1" (Just "v1"),ExtensionHeader "h2" (Just "v2")]
  r6
  "r6 chunked body trailer is as expected"

tr6ChunkedBodySingle =
  assertThatEq
  (length . cbChunks . hrBody)
  1
  r6
  "r6 chunked body has 1 chunk"

tr6ChunkExtension =
  assertThatEq
  (fst . head . cbChunks . hrBody)
  [("extNothing",Nothing),("ext1",Just "I")]
  r6
  "Extensions of r6 single chunk are as expected"

tr6ChunkData =
  assertThatEq
  (snd . head . cbChunks . hrBody)
  "1234567890123456789012345"
  r6
  "Data of r6 single chunk is as expected"

t12 = return $ fromRightOrError
  $ assert . (== ["a", "b", "c", "d"])
    <$> parse (list1 token) "" "  a,b , c, d"
    <*> pure "Parse list of tokens"

tParseTextCrlf = return $ fromRightOrError
  $ assert . (== "abc def")
    <$> parse text "" "abc\r\n def\r\n" <*> pure "parse text followed by crlf"

tests = (do
  tms <- tMethods
  tss <- tSchemes
  tas <- tAuthorities
  tps <- tPaths
  tqs <- tQueries
  ts  <- sequence [tr4IsLeft, tr3ContentLength, tr1Headers,
                   tr6Headers, tr6ChunkedBodyTrailer,
                   tr6ChunkedBodySingle, tr6ChunkExtension,
                   tr6ChunkData,
                   t6, t8, t9, t10, t11, t12, tParseTextCrlf]
  return $ tms ++ tss ++ tas ++ tps ++ tqs ++ ts)
  >>= mapM_ putStrLn
