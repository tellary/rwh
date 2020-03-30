import RandomSupply
import Test.Hspec

supplyTimesSupply :: (MonadSupply s m, Num s) => m s
supplyTimesSupply = do
  s1 <- supply
  s2 <- supply
  return (s1 * s2)

main = hspec $ do
  supplySpec
    "IntList"
    (runListSupply :: ListSupply Int Int
                   -> [Int] -> Maybe (Int, [Int]))

  supplySpec
    "DoubleList"
    (runListSupply :: ListSupply Double Double
                   -> [Double] -> Maybe (Double, [Double]))

  supplySpec
    "IntListState"
    (runListStateSupply :: ListStateSupply Int Int
                   -> [Int] -> Maybe (Int, [Int]))

supplySpec name run = do
  describe (name ++ "Supply") $ do
    it "multiplies two supplies correctly" $ do
      run supplyTimesSupply [2, 3, 4]
        `shouldBe` Just (6, [4])
    it "returns Nothing when list is consumed" $ do
      run supplyTimesSupply [2]
        `shouldBe` Nothing
