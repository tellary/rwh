module Resize (byteStringJSDataUrl) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Monad.IO.Class  (liftIO)
import Data.ByteString         (ByteString)
import GHCJS.Buffer            (fromByteString, getArrayBuffer, thaw)
import GHCJS.DOM.Blob          (newBlob)
import GHCJS.DOM.EventM        (on)
import GHCJS.DOM.FileReader    (getResult, load, newFileReader, readAsDataURL)
import GHCJS.DOM.Types         (ArrayBuffer (..), BlobPropertyBag)
import GHCJS.Marshal.Pure      (pToJSVal)
import GHCJS.Types             (JSString, JSVal)
import Miso                    (JSM, ghcjsPure)
import Unsafe.Coerce           (unsafeCoerce)

byteStringJSBuffer :: ByteString -> JSM JSVal
byteStringJSBuffer bs = do
  (buffer, _, _) <- ghcjsPure $ fromByteString bs
  arr <- ghcjsPure =<< (getArrayBuffer <$> thaw buffer)
  return . pToJSVal $ arr

byteStringBlob bs = do
  buf <- byteStringJSBuffer bs
  newBlob [ArrayBuffer buf] (Nothing :: Maybe BlobPropertyBag)

byteStringJSDataUrl :: ByteString -> JSM JSString
byteStringJSDataUrl bs = do
  r <- newFileReader
  b <- byteStringBlob bs
  readAsDataURL r (Just b)
  v <- liftIO $ newEmptyMVar
  on r load $ do
    result <- getResult r
    liftIO $ putMVar v result
  Just result <- liftIO . readMVar $ v
  return . unsafeCoerce . pToJSVal $ result
