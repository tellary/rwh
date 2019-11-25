import Text.Read.Lex (Lexeme(Symbol))
import Text.Read

newtype Expression = Expression Int deriving Show

result (Expression n) = n

readNum = do
  n <- readPrec
  return $ Expression n

readSumR = do
  n1 <- readPrec
  Symbol "+" <- lexP
  Expression n2 <- step readSum
  return $ Expression $ n1 + n2

readSumSumL = do
  Punc "(" <- lexP
  Expression sumL <- readSum
  Punc ")" <- lexP
  Symbol "+" <- lexP
  Expression sumR <- readSum
  return $ Expression $ sumL + sumR

-- sum -> ([`(`] num + sum | num [`)`]) | `( sum )` + sum
readSum = (parens $ readSumR +++ readNum) +++ readSumSumL

instance Read Expression where
  readPrec = readSum


-- result $ read "(1 + 2) + 4"
-- result $ read "1 + (2 + 4)"
-- result $ read "(1 + 2 + 4)"
-- readPrec_to_S readSum 0 "(1 + 2) + 3 + 4"
