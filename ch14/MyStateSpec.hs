import MyState

import Test.Hspec

main = hspec $ do
  describe "MyStateT" $ do
    it "is correct as applictive 1 + 2" $
      myRunStateT ((+) <$> pure 1 <*> pure 2) 0 `shouldReturn` (3, 0)

    it "does fmap (*3) correctly" $
      myRunStateT (fmap (*3) $ return 2) 0 `shouldReturn` (6, 0)

    it "updates state correctly" $ do
      let s = do
            v <- (+) <$> pure 1 <*> pure 2
            s <- get
            put (s*2)
            return v
      myRunStateT s 3 `shouldReturn` (3, 6)
