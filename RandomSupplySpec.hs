import RandomSupply
import Test.Hspec

supplyTimesSupply :: (MonadSupply s m, Num s) => m s
supplyTimesSupply = do
  s1 <- supply
  s2 <- supply
  return (s1 * s2)

main = hspec $ do
  describe "ListSupply" $ do
    it "multiplies two supplies correctly" $ do
      runListSupply supplyTimesSupply [2, 3, 4]
        `shouldBe` (Just (6, [4]) :: Maybe (Int, [Int]))
    it "returns Nothing when list is consumed" $ do
      runListSupply supplyTimesSupply ([2] :: [Int])
        `shouldBe` (Nothing :: Maybe (Int, [Int]))

  describe "ListStateSupply" $ do
    it "multiplies two supplies correctly" $ do
      runListStateSupply supplyTimesSupply [2, 3, 4]
        `shouldBe` (Just (6, [4]) :: Maybe (Int, [Int]))
    it "returns Nothing when list is consumed" $ do
      runListStateSupply supplyTimesSupply ([2] :: [Int])
        `shouldBe` (Nothing :: Maybe (Int, [Int]))
