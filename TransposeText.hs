import InteractWith
import Data.List

toList :: [Maybe a] -> Maybe [a]
toList = foldr step (Just [])
  where step x r = do
          x1 <- x
          r1 <- r
          return $ x1:r1

zipList f = toList . zipListMaybe (fmap f . toList)
myTranspose = zipList id

zipListMaybe f [] = []
zipListMaybe f xss = f head:zipListMaybe f tail
  where (head, tail) = headTailMaybe xss

headTailMaybe xss
  | empty = (head, [])
  | otherwise = (head,  tail)
  where
    (head, tail, empty) = foldr step ([], [], True) $ map uncons xss

step (Just (h, t)) (hs, ts, empty) =
  ((Just h):hs, t:ts, null t && empty)
step Nothing (hs, ts, empty) = (Nothing:hs, []:ts, empty)

myTransposeText = fmap unlines . myTranspose . lines
myTransposeTextOrError text =
  case myTransposeText text of
    Just ttext -> ttext
    _ -> error "Input text isn't rectangular"

main = mainWith myTransposeTextOrError
