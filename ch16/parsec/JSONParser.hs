module JSONParser where

import Text.ParserCombinators.Parsec
import SimpleJSON

jsonFile = json <* eof

json :: CharParser st JValue
json = spaces *> (object <|> array) <* spaces

object      = between (char '{') (char '}')
              (JObject . JObj <$> fieldValues)
fieldValues = sepBy fieldValue (char ',')
fieldValue  = (,) <$> field <* char ':' <*> value
field       = spaces *> quotedString <* spaces

value =
  spaces *> (
        JNull       <$  string "null"
    <|> JBool True  <$  string "true"
    <|> JBool False <$  string "false"
    <|> JString     <$> quotedString
    <|> JNumber     <$> number
    <|> json)
  <* spaces

quotedString = between (char '"') (char '"') (many $ noneOf "\"")

number :: CharParser st Double
number = do
  minus <- option ""  (string "-")
  whole <- many1 digit
  dot   <- option "." (string ".")
  frac  <- option "0" (many1 digit)
  return $ read $ concat [minus, whole, dot, frac]

array       = between (char '[') (char ']')
              (JArray . JAry <$> arrayValues)
arrayValues = sepBy value (char ',')
