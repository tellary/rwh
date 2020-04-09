{-# LANGUAGE FlexibleContexts #-}

module URLQueryParser where

import Numeric (readHex)
import Text.Parsec.Prim (ParsecT, Stream)
import Text.ParserCombinators.Parsec hiding (try)

query1 :: Stream s m Char => ParsecT s u m [(String, Maybe String)]
query1 = query <* eof
query :: Stream s m Char => ParsecT s u m [(String, Maybe String)]
query = sepBy fieldValue (char '&')
fieldValue :: Stream s m Char => ParsecT s u m (String, Maybe String)
fieldValue = (,) <$> field <*> optionMaybe (char '=' *> value)

urlChar :: Stream s m Char => ParsecT s u m Char
urlChar =
      directChar
  <|> spaceChar
  <|> hexChar

isDirectChar c =
     c `elem` ['A'..'Z']
  || c `elem` ['a'..'z']
  || c `elem` ['0'..'9']
  || c `elem` "*-._"

directChar, spaceChar, hexChar :: Stream s m Char => ParsecT s u m Char
directChar = satisfy isDirectChar
spaceChar  = (char '+') *> pure ' '
hexChar    = char '%' *>
             (toEnum . fst . head . readHex <$> count 2 hexDigit)

field, value :: Stream s m Char => ParsecT s u m String
field = many urlChar
value = many urlChar

-- λ> parse query "" $ UnlimitedStream "field1=value1+2+%21&field2=value2%22&field3&field1&field1="
-- Right [("field1",Just "value1 2 !"),("field2",Just "value2\""),("field3",Nothing),("field1",Nothing),("field1",Just "")]
