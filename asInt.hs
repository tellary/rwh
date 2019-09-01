import Data.Char
import Data.List

data AsIntError = AsIntEmpty |
                  AsIntMinus |
                  AsIntNotDigit Char |
                  AsIntOverflow Int Char deriving (Show)

type AsInt = Either AsIntError Int

asInt :: String -> AsInt
asInt "" = Left AsIntEmpty
asInt "-" = Left AsIntMinus
asInt ('-':s) =
  case asInt s of
    (Right v) -> Right (-v)
    e@(Left _) -> e
asInt s = foldl' step (Right  0) s
  where step (Right a) c
          | not $ isDigit c = Left $ AsIntNotDigit c
          | d > a = Right d
          | otherwise = Left $ AsIntOverflow a c
          where d = a*10 + digitToInt c
        step e@(Left _) _ = e
          -- Overflow may also be fixed by writing
          -- a `String -> Integer` function, but
          -- Assignement requires a `-> Int` one.
          --
          -- where d = a*10 + toInteger (digitToInt c)
