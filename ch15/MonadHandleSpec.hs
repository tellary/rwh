import           Control.Monad (when)
import           HandleIO
import           LogIO
import           MonadHandle
import           System.Directory
import           System.IO (IOMode(..))
import qualified System.IO as S
import           Test.Hspec hiding (runIO)

writeHello :: MonadHandle h m => m ()
writeHello = do
  f <- openFile "helloFile" WriteMode
  hPutStr f "Hello"
  hPutStr f "\n"
  hClose  f

withoutFile :: FilePath -> IO () -> IO ()
withoutFile f a = do
  exists <- doesPathExist f
  when exists $ removeFile f
  a
  removeFile f

main = hspec $ do
  describe "LogIO" $ do
    it "produces correct log" $
      execLogIO writeHello `shouldBe`
      [Open  "helloFile" WriteMode,
       Put   "helloFile" "Hello",
       Put   "helloFile" "\n",
       Close "helloFile"]

  describe "HandleIO" $ do
    it "writes correct file" $ withoutFile "helloFile" $ do
        runIO writeHello
        f <- S.openFile "helloFile" ReadMode
        S.hGetContents f
      `shouldReturn` "Hello\n"
