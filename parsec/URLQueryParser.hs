module URLQueryParser where

import Numeric (readHex)
import Text.ParserCombinators.Parsec

query1 = query <* eof
query :: CharParser st [(String, Maybe String)]
query = sepBy fieldValue (char '&')
fieldValue :: CharParser st (String, Maybe String)
fieldValue = (,) <$> field <*> optionMaybe (char '=' *> value)

urlChar =
      directChar
  <|> spaceChar
  <|> hexChar

isDirectChar c =
     c `elem` ['A'..'Z']
  || c `elem` ['a'..'z']
  || c `elem` ['0'..'9']
  || c `elem` "*-._"

directChar = satisfy isDirectChar
spaceChar  = (char '+') *> pure ' '
hexChar    = char '%' *>
             (toEnum . fst . head . readHex <$> count 2 hexDigit)

field = many urlChar
value = many urlChar

-- λ> parse query "" "field1=value1+2+%21&field2=value2%22&field3"
-- Right [("field1",Just "value1 2 !"),("field2",Just "value2\""),("field3",Nothing)]
