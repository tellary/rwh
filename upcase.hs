import System.IO
import Data.Char

main1 = interact (map toUpper)
main2 = interact (unlines . map firstToUpper . lines)
  where firstToUpper [] = []
        firstToUpper (c:str) = toUpper c:str

main = main2        
