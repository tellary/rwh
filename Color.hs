import Data.Char(isSpace)

data Color = Red | Green | Blue

instance Eq Color where
  Red == Red = True
  Green == Green = True
  Blue == Blue = True
  _ == _ = False

instance Show Color where
  show Red = "Red"
  show Green = "Green"
  show Blue = "Blue"

instance Read Color where
  readsPrec _ value = tryParse matches
    where matches =
            [("Red", Red), ("Green", Green), ("Blue", Blue)]
          tryParse [] = []
          tryParse (m:ms) =
            case m of
              (match, result) ->
                if (take (length match) value' == match)
                then [(result, drop (length match) value')]
                else tryParse ms
                where value' = dropWhile isSpace value
