import Text.ParserCombinators.Parsec

p1 :: CharParser st String
p1 = a1 <* spaces <* eof
a1 = string "a"

p2 :: CharParser st String
p2 = a2 <* eof
a2 = string "a" <* spaces
