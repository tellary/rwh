import           Control.Exception      (fromException)
import           Control.Monad          (when)
import           Control.Monad.Catch    (bracket, handle)
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.Either            (fromRight)
import           HandleIO
import           LogIO
import           MonadHandle
import           System.Directory
import           System.IO              (IOMode(..))
import qualified System.IO as S
import           System.IO.Error        ( ioeGetErrorType
                                        , illegalOperationErrorType)
import           Test.Hspec hiding      (runIO)

writeHello :: MonadHandle h m => m ()
writeHello =
  bracket
  (openFile "helloFile" WriteMode)
  hClose $ \f -> do
    hPutStr f "Hello"
    hPutStr f "\n"

writeHelloAndTidyUp :: (MonadIO m, MonadHandle h m) => m ()
writeHelloAndTidyUp = do
  writeHello
  liftIO $ removeFile "helloFile"

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

    it "throws illegal operation on `hPutStr` on a read-only handle" $
      let e = either fromException undefined . execLogIO $ do
            f <- openFile "readOnly" ReadMode
            hPutStr f "something"
      in
        ioeGetErrorType <$> e `shouldBe` Just illegalOperationErrorType

    it "catches illegal operation on a read-only handle" $
      let errorType = handle (return . Just . ioeGetErrorType) (do
            f <- openFile "readOnly" ReadMode
            hPutStr f "something"
            return Nothing)
      in (fromRight undefined . evalLogIO $ errorType)
         `shouldBe` Just illegalOperationErrorType

  describe "HandleIO" $ do
    it "writes correct file" $ withoutFile "helloFile" $ do
        runIO writeHello
        f <- S.openFile "helloFile" ReadMode
        S.hGetContents f
      `shouldReturn` "Hello\n"

    it "removes file in `liftIO`" $ do
        runIO writeHelloAndTidyUp
        doesPathExist "helloFile"
      `shouldReturn` False
