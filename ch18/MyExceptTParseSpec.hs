import MyExceptTParse
import Test.Hspec

main = hspec $ do
  describe "parse" $ do
    it "parses a char" $
      parse (char 'a') "abc"
      `shouldBe`
      (Right 'a', ParseState { stOffset = 1, stString = "bc" })

    it "parses many chars" $
      parse (many $ char 'a') "aaabc"
      `shouldBe`
      (Right "aaa", ParseState { stOffset = 3, stString = "bc" })

    it "parses empty result when many chars are not present" $
      parse (many $ char 'a') "bc"
      `shouldBe`
      (Right "", ParseState { stOffset = 0, stString = "bc" })

    it "parses some chars" $
      parse (some $ char 'a') "aaabc"
      `shouldBe`
      (Right "aaa", ParseState { stOffset = 3, stString = "bc" })

    it "fails to parse some chars when some chars are not present" $
      parse (some $ char 'a') "bc"
      `shouldBe`
      (Left "Unexpected char 'b' while 'a' is expected",
       ParseState { stOffset = 0, stString = "bc" })

    it "parses a string" $
      parse (string "foo") "foobar"
      `shouldBe`
      (Right "foo", ParseState { stOffset = 3, stString = "bar" })

    it "fails to parse a mismatching string" $
      parse (string "foo") "bar"
      `shouldBe`
      (Left "Unexpected string \"bar\" while \"foo\" is expected",
       ParseState { stOffset = 0, stString = "bar" })

    it "parses foo in (foo | bar)" $
      parse (string "foo" <|> string "bar") "foobar"
      `shouldBe`
      (Right "foo", ParseState { stOffset = 3, stString = "bar" })

    it "parses bar in (foo | bar)" $
      parse (string "foo" <|> string "bar") "bar"
      `shouldBe`
      (Right "bar", ParseState { stOffset = 3, stString = "" })

    it "fails to parse fo by (foo | bar) " $
      parse (string "foo" <|> string "bar") "fo"
      `shouldBe`
      (Left "Unexpected string \"fo\" while \"bar\" is expected",
       ParseState { stOffset = 0, stString = "fo" })
