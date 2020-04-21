{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.Reader (MonadReader (..), Reader, runReader)
import Control.Monad.State  (MonadState (..), State, runState)
import Control.Monad.Writer (MonadWriter (..), Writer, runWriter)
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

newtype MyExceptWriter w a = MEW (MyExceptT String (Writer w) a)
  deriving (Functor, Applicative, Monad, MonadWriter w)

runMyExceptWriter (MEW m) = runWriter . runMyExceptT $ m

outputIfWriterIsEmpty :: MonadWriter String m => m a -> m a
outputIfWriterIsEmpty m = pass $ do
  a <- m
  return (a, \out ->
               if null out
               then "empty"
               else "non empty: " ++ out)

newtype MyExceptState s a = MES (MyExceptT String (State s) a)
  deriving (Functor, Applicative, Monad, MonadState s)

runMyExceptState (MES m) s = (`runState` s) . runMyExceptT $ m

doubleState :: MonadState Int m => m ()
doubleState = do
  v <- get
  put (2*v)

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

  describe "MyExceptWriter" $ do
    it "tells if writer is empty" $
      (runMyExceptWriter . outputIfWriterIsEmpty . return $ 1)
      `shouldBe` (Right 1, "empty")
    it "tells if writer is not empty" $
      (runMyExceptWriter . outputIfWriterIsEmpty $ tell "test" >> return 1)
      `shouldBe` (Right 1, "non empty: test")
    it "listens correctly" $
      (runMyExceptWriter . listen $ tell "test" >> return 1)
      `shouldBe` (Right (1, "test"), "test")
    it "tells correctly" $
      (runMyExceptWriter $ tell "test" >> return 1)
      `shouldBe` (Right 1, "test")
    it "handles left value in the chain" $
      (runMyExceptWriter
       $ tell "test" >> MEW (MyExceptT . return . Left $ "error") >> return 1)
      `shouldBe` (Left "error", "test")

  describe "MyExceptState" $ do
    it "doubles int state correctly" $
      runMyExceptState doubleState 2
      `shouldBe` (Right (), 4)
