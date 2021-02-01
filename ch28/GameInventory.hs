module GameInventory where

removeInv _ [] = Nothing
removeInv e (x:xs)
  | e /= x = (x:) <$> removeInv e xs
  | otherwise = Just xs
