module CSV where

import Text.ParserCombinators.Parsec

csv = sepBy line newline <* eof
line = sepBy cell (char ',')
cell = quotedValue <|> value
quotedValue = between (char '"') (char '"') quotedChars
quotedChars = many quotedChar
quotedChar = noneOf "\"" <|> (try (string "\"\"") *> pure '"')
value :: CharParser st String
value = many $ noneOf ",\n\r"

-- λ> parse csv "" "\nabc,cde\n,\"\"\"quoted\"\" string\""
-- Right [[""],["abc","cde"],["","\"quoted\" string"]]
