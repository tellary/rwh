import LogIO
import MonadHandle
import System.IO (IOMode(..))
import Test.Hspec

writeHello :: MonadHandle h m => m ()
writeHello = do
  f <- openFile "helloFile" WriteMode
  hPutStr f "Hello"
  hPutStr f "\n"
  hClose  f

main = hspec $ do
  describe "writeHello" $ do
    it "produces correct log" $
      execLogIO writeHello `shouldBe`
      [Open  "helloFile" WriteMode,
       Put   "helloFile" "Hello",
       Put   "helloFile" "\n",
       Close "helloFile"]
