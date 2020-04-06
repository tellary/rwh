import           Control.Exception (fromException)
import           Control.Monad     (when)
import           Data.Either       (fromRight)
import           HandleIO
import           LogIO
import           MonadHandle
import           System.Directory
import           System.IO         (IOMode(..))
import qualified System.IO as S
import           System.IO.Error   (ioeGetErrorType, illegalOperationErrorType)
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
      fromRight undefined (execLogIO writeHello) `shouldBe`
      [ Open  "helloFile" WriteMode
      , Put   (LogIOHandle "helloFile" WriteMode) "Hello"
      , Put   (LogIOHandle "helloFile" WriteMode) "\n"
      , Close (LogIOHandle "helloFile" WriteMode)
      ]

    it "throws on hPutStr in a read-only handle" $
      let e = either fromException undefined . execLogIO $ do
            f <- openFile "readOnly" ReadMode
            hPutStr f "something"
      in
        ioeGetErrorType <$> e `shouldBe` Just illegalOperationErrorType

  describe "HandleIO" $ do
    it "writes correct file" $ withoutFile "helloFile" $ do
        runIO writeHello
        f <- S.openFile "helloFile" ReadMode
        S.hGetContents f
      `shouldReturn` "Hello\n"
