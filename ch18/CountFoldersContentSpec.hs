import           Control.Exception   (catch)
import           CountFoldersContent
import qualified Data.Map            as M
import           System.Directory    (createDirectory)
import           System.IO           (writeFile)
import           System.IO.Temp      (withTempDirectory)
import           Test.Hspec

initTestDirectory dir = do
  let fn f = dir ++ "/" ++ f
  writeFile (fn "1") ""
  writeFile (fn "2") ""
  createDirectory $ fn "empty"
  createDirectory $ fn "l1"
  writeFile (fn "l1/11") ""
  createDirectory $ fn "l1/l12"
  writeFile (fn "l1/l12/121") ""
  createDirectory $ fn "l2"
  writeFile (fn "l2/21") ""
  writeFile (fn "l2/22") ""
  writeFile (fn "l2/23") ""

withTestDirectory a
  = withTempDirectory "." "test" $ \dir -> do
      initTestDirectory dir
      a dir

main = hspec $ around withTestDirectory $ do
  describe "runCountFoldersContent" $ do
    it "counts contents correctly" $ \dir ->
      let fn f = dir ++ "/" ++ f in do
        runCountFoldersContent 200 dir
      `shouldReturn`
        ( M.fromList
          [ (dir        , 5)
          , (fn "empty" , 0)
          , (fn "l1"    , 2)
          , (fn "l1/l12", 1)
          , (fn "l2"    , 3)
          ]
        , AppState { stMaxDepth = 2 }
        )

    it "handles max depth 0 as `find` does" $ \dir ->
      runCountFoldersContent 0 dir
      `shouldReturn` (M.fromList [(dir, 5)], AppState { stMaxDepth = 0 })

    it "respects max depth" $ \dir ->
      let fn f = dir ++ "/" ++ f in do
        runCountFoldersContent 1 dir
      `shouldReturn`
        ( M.fromList
          [ (dir        , 5)
          , (fn "empty" , 0)
          , (fn "l1"    , 2)
          , (fn "l2"    , 3)
          ]
        , AppState { stMaxDepth = 1 }
        )

    it "throws on unexistent directory" $ \_ -> do
        runCountFoldersContent 10 "unexistent"
        return "shouldn't happen"
      `catch` (\e -> return $ show (e :: IOError))
      `shouldReturn`
      "unexistent: getDirectoryContents:openDirStream: does not exist (No such file or directory)"

    it "throws on an attempt to count on a file" $ \dir -> do
        runCountFoldersContent 10 (dir ++ "/1")
        return "shouldn't happen"
      `catch` (\e -> return $ show (e :: IOError))
      `shouldReturn`
      dir ++ "/1: getDirectoryContents:openDirStream: inappropriate type (Not a directory)"
