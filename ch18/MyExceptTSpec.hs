{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.Reader (Reader, MonadReader(..), runReader)
import MyExceptT
import Test.Hspec

newtype MyExceptReader r a = MER (MyExceptT String (Reader r) a)
  deriving (Functor, Applicative, Monad, MonadReader r)

runMyExceptReader :: MyExceptReader r a -> r -> Either String a
runMyExceptReader (MER m) r = (`runReader` r) . runMyExceptT $ m

localDemo :: MonadReader String m => m (String, String, String)
localDemo = do
  r1 <- myName 1
  r2 <- local (++ " the Great") (myName 2)
  r3 <- myName 3
  return (r1, r2, r3)
  where myName step = do
          name <- ask
          return $ "Step " ++ show step  ++ ": Hello, " ++ name ++ "!"

main = hspec $ do
  describe "MyExcept" $ do
    it "binds (+1) correctly" $
      runMyExcept (return 1 >>= return . (+1))
      `shouldBe` (Right 2 :: Either String Int)

  describe "MyExceptReader" $ do
    it "returns `ask` correctly" $
      runMyExceptReader ask 8 `shouldBe` Right 8
    it "runs `localDemo` correctly" $
      runMyExceptReader localDemo "Peter"
      `shouldBe`
      Right ( "Step 1: Hello, Peter!"
            , "Step 2: Hello, Peter the Great!"
            , "Step 3: Hello, Peter!"
            )
