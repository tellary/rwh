import InteractWith

firstLineWord text =
  unlines $ map firstWord (lines text)
  where
    firstWord [] = []
    firstWord line = head $ words line

main = mainWith firstLineWord
