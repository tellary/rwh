import           Control.Exception      (SomeException, fromException,
                                         toException)
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

writeToReadOnlyHandle = do
  f <- openFile "readOnly" ReadMode
  hPutStr f "something"

writeToClosedHandle :: MonadHandle h m => m SomeException
writeToClosedHandle = handle return $ do
    f <- openFile "writeFile" WriteMode
    hClose  f
    hPutStr f "something"
    return . toException . userError $ "shouldn't get to this statement"

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
      [ Open  "helloFile" WriteMode
      , Put   "helloFile" "Hello"
      , Put   "helloFile" "\n"
      , Close "helloFile"
      ]

    it "throws an illegal operation on `hPutStr` on a read-only handle" $
      let e = either fromException undefined . evalLogIO
            $ writeToReadOnlyHandle
      in
        ioeGetErrorType <$> e `shouldBe` Just illegalOperationErrorType

    it "saves log before exception" $
      execLogIO writeToReadOnlyHandle `shouldBe`
      [ Open "readOnly" ReadMode ]

    it "catches an illegal operation on a read-only handle" $
      let errM    = handle return (do
            f <- openFile "readOnly" ReadMode
            hPutStr f "something"
            return $ userError "shouldn't happen")
          err     = (fromRight undefined . evalLogIO $ errM)
      in show err
         `shouldBe`
         "readOnly: hPutStr: illegal operation (handle is in read mode)"

    it "catches an illegal operation on a closed handle" $
      let err = fromRight undefined . evalLogIO $ writeToClosedHandle
      in show err
         `shouldBe`
         "writeFile: hPutStr: illegal operation (handle is closed)"

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

    it "catches an illegal operation on a closed handle" $ do
        err <- runIO $ writeToClosedHandle
        removeFile "writeFile"
        return . show $ err
      `shouldReturn`
      "writeFile: hPutStr: illegal operation (handle is closed)"
