import           Data.Char        (isSpace)
import           Control.Monad    (forM_)
import           Prettifier
import           Test.QuickCheck

printChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ']
printString = listOf printChar

instance Arbitrary Doc where
  arbitrary = oneof [
    text <$> printString,
    char <$> printChar,
    return line,
    (</>) <$> arbitrary <*> arbitrary,
    (<>)  <$> arbitrary <*> arbitrary,
    return empty,
    return indent,
    return outdent
    ]

prop_widthSameNoSpace :: Int -> Doc -> Property
prop_widthSameNoSpace w doc =
  w > 0 ==> compactText == widthText
  where
    compactText = filter (not . isSpace) $ compact doc
    widthText   = filter (not . isSpace) $ width w doc

prop_fillSameNoSpace :: Int -> Doc -> Property
prop_fillSameNoSpace w doc =
  w > 0 ==> compactText == fillText
  where
    compactText = filter (not . isSpace) $ compact        doc
    fillText    = filter (not . isSpace) $ fillChar ' ' w doc

prop_indentedSameNoSpace :: Int -> Doc -> Property
prop_indentedSameNoSpace i doc =
  i > 0 ==> compactText == indentedText
  where
    compactText  = filter (not . isSpace) $ compact    doc
    indentedText = filter (not . isSpace) $ indented i doc

args = stdArgs { maxSuccess = 200 }

tests = [prop_widthSameNoSpace, prop_fillSameNoSpace,
         prop_indentedSameNoSpace]

main = do
  forM_ tests $ quickCheckWith args
