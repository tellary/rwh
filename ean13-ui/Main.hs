{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import           Control.Concurrent            (ThreadId, killThread)
import           Control.Concurrent.Async      (async, asyncThreadId, wait)
import           Control.Exception             (throw)
import           Control.Monad                 (when)
import           Data.List                     (intercalate)

import           Codec.Picture                 (Image, PixelRGB8)
import           Control.Concurrent.MVar       (newEmptyMVar, putMVar, readMVar)
import           Control.Exception             (SomeException, catch,
                                                displayException)
import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as B (length)
import qualified Data.ByteString.Char8         as C (pack)
import           GHCJS.Foreign                 (isUndefined)
import           GHCJS.Foreign.Callback        (Callback)
import           GHCJS.Types                   (JSVal)
import           Helper                        (createDataUrl, ean13,
                                                errorCutoff,
                                                parseImageByteString,
                                                parseImageDataUrl)
import           JavaScript.Web.XMLHttpRequest (Method (GET), Request (..),
                                                RequestData (NoData),
                                                Response (contents),
                                                xhrByteString)
import           Miso                          (App (..), Effect (Effect), View,
                                                accept_, asyncCallback, br_,
                                                button_, consoleLog,
                                                defaultEvents, div_,
                                                getElementById, id_, img_,
                                                input_, noEff, onChange,
                                                onClick, placeholder_, src_,
                                                startApp, text, type_, (<#))
import           Miso.String                   (MisoString, append,
                                                fromMisoString, ms)
import           Model                         (BarcodeStage (..), EAN13 (..),
                                                ImageDataUrl (ImageDataUrl,
                                                              imageDataUrl),
                                                Model (Model, imageUrl,
                                                       stage, threadId),
                                                Result (Bad, Good),
                                                UIException (UIException))

data Action
  = NoOp
  | UpdateImageUrl MisoString
  | SetThread ThreadId (IO Action)
  | ReadImage
  | FetchImage
  | SetDataUrl ImageDataUrl
  | SetImageBytes ByteString
  | SetImage (Image PixelRGB8) ImageDataUrl
  | SetError MisoString
  | SetResult Result

noBarcodeChosen = "No barcode image chosen"
app = App { model         = Model "" Nothing (ErrorStage $ ms noBarcodeChosen)
          , initialAction = NoOp
          , update        = updateModel
          , view          = viewModel
          , events        = defaultEvents
          , subs          = []
          , mountPoint    = Nothing
          }

main = startApp app

maybeKillJob m =
  case threadId m of
    -- We keep track of a thread that does heavy lifting and
    -- try to kill it before starting next barcode recognition job.
    -- This takes care of a scenario when a long running job is started,
    -- then we choose a different image that's processed faster and
    -- a result of the faster processing is later overridden when the
    -- long running job completes.
    Just tid -> do
      consoleLog . ms $ "Killing existing job " ++ show tid
      killThread tid
    _ -> return ()

fetchImage :: MisoString -> IO ByteString
fetchImage url =
  exLog "fetch image" $ contents <$> xhrByteString req >>= \case
      Nothing -> throw $ UIException "Failed to fetch image"
      Just bs -> return bs
  where
    req = Request { reqMethod          = GET
                  , reqURI             = fromMisoString $ url
                  , reqLogin           = Nothing
                  , reqHeaders         = []
                  , reqWithCredentials = False
                  , reqData            = NoData
                  }

sizeLimit :: Int
sizeLimit = 1024*256

checkSize :: Int -> IO ()
checkSize size = do
  consoleLog . ms $ "File size: " ++ show size
  when (size > sizeLimit)
    $ throw . UIException
    $ "Image file is too big, 256k max. It's "
    ++ show (size `div` 1024) ++ "k."

exLog :: String -> IO a -> IO a
exLog msg a
  = catch a $ \(e :: SomeException) -> do
      consoleLog .  ms $ "Failed to " ++ msg ++ ": " ++ displayException e
      throw e

exAction :: IO Action -> IO Action
exAction a
  = catch a $ \(e :: SomeException) -> do
      return . SetError . ms $ displayException e

readImageFromFile :: IO ImageDataUrl
readImageFromFile = do
  fileReaderInput <- getElementById "fileReader"
  file            <- getFile fileReaderInput
  getSize file >>= \case
    Nothing   -> throw . UIException $ noBarcodeChosen
    Just size -> do
      checkSize size
      reader    <- newReader
      imageMVar <- newEmptyMVar
      setOnLoad reader =<< asyncCallback (do
        r <- ImageDataUrl <$> getResult reader
        putMVar imageMVar $ r)
      readDataURL reader file
      readMVar imageMVar

parseFileImage :: ImageDataUrl -> IO (Image PixelRGB8)
parseFileImage dataUrl
  = case parseImageDataUrl . fromMisoString . imageDataUrl $ dataUrl of
      Right img -> return img
      Left err  -> throw . UIException $ err              

parseFetchedImage
  :: ByteString
  -> IO (Image PixelRGB8, ImageDataUrl)
parseFetchedImage bs =
  case parseImageByteString bs of
    Left  errs
      -> throw . UIException
      $  "Image isn't of a supported type.\n"
      ++ "Please not that .png isn't supported.\n"
      ++ "The following types were tried:\n"
      ++ intercalate ",\n" (map showErr errs)
      where showErr (t, err) = t ++ ": " ++ err
    Right (imgType, img)
      -> return (img, ImageDataUrl . ms . createDataUrl (C.pack imgType) $ bs)

recognizeBarcode :: Image PixelRGB8 -> IO Result
recognizeBarcode img
  = case ean13 img of
      Right r  -> return $ if eanError r < errorCutoff
                          then Good r
                          else Bad  r
      Left err -> throw . UIException $ err


updateStage :: Action -> BarcodeStage -> Effect Action BarcodeStage
updateStage (SetDataUrl dataUrl) _
  = ImageDecodingStage (Just dataUrl) <# do
      job <- async . parseFileImage $ dataUrl
      return . SetThread (asyncThreadId job) . exAction $ do
        img <- wait job
        return $ SetImage img dataUrl
updateStage (SetImageBytes bs) _
  = ImageDecodingStage Nothing <# do
      job <- async . parseFetchedImage $ bs
      return . SetThread (asyncThreadId job) . exAction $ do
        (img, dataUrl) <- wait job
        return $ SetImage img dataUrl
updateStage (SetImage img dataUrl) _
  = BarcodeRecognitionStage dataUrl <# do      
      job <- async . recognizeBarcode $ img
      return . SetThread (asyncThreadId job) . exAction $ do
        r <- wait job
        return $ SetResult r
updateStage (SetResult r) (BarcodeRecognitionStage dataUrl)
  = noEff $ ResultStage r dataUrl
updateStage (SetResult _) s
  = noEff s
updateStage (SetError err) (BarcodeRecognitionStage dataUrl)
  = noEff $ ErrorImageStage err dataUrl
updateStage (SetError err) _
  = noEff $ ErrorStage err
updateStage _ _ = updateStageInvocationError

updateStageInvocationError =
  throw . UIException $ "`updateStage` invoked for a wrong Action"
  
updateModel :: Action -> Model -> Effect Action Model
updateModel ReadImage m
  = m { stage = ImageReadingStage } <# do
      exAction $ do
        maybeKillJob m
        dataUrl <- readImageFromFile
        return . SetDataUrl $ dataUrl
updateModel FetchImage m
  = m { stage = ImageFetchingStage } <# do
      maybeKillJob m
      job <- async . fetchImage . imageUrl $ m
      return . SetThread (asyncThreadId job) . exAction $ do
        bs <- wait job
        checkSize (B.length bs)
        return . SetImageBytes $  bs
updateModel (UpdateImageUrl url) m
  = m { imageUrl = url } <# do
      consoleLog ("url: " `append` url)
      return NoOp
updateModel (SetThread thId act) m = m { threadId = Just thId } <# act
updateModel NoOp m = noEff m
updateModel a m
  = Effect m { stage = s } acts
  where Effect s acts = updateStage a (stage m)

viewModel :: Model -> View Action
viewModel m
  = div_ [] $ [
    "Barcode recognition"
    , br_ []
    , input_ [ id_ "fileReader"
             , type_ "file"
             , accept_ "image/*"
             , onChange (const ReadImage)
             ]
    , br_ []
    , input_ [ placeholder_ "Barcode image URL"
             , type_ "url"
             , onChange UpdateImageUrl
             ]
    , button_ [ onClick FetchImage ] [ text "Read barcode"]
    , br_ []
    ] ++ modelView m
  where
    modelView m = stageView . stage $ m

    stageView ImageReadingStage
      = [ text "Loading image from file ..." ]
    stageView ImageFetchingStage
      = [ text "Fetching image from url ..." ]
    stageView (ImageDecodingStage Nothing)
      = [ text "Decoding image ..." ]
    stageView (ImageDecodingStage (Just img))
      = [ text "Decoding image ...", br_ []
        , img_ [src_ . imageDataUrl $ img]
        ]
    stageView (BarcodeRecognitionStage img)
      = [ text "Recognizing EAN13 barcode ...", br_ []
        , img_ [src_ . imageDataUrl $ img]
        ]
    stageView (ErrorStage err)
      = [ text err ]
    stageView (ErrorImageStage err img)
      = [ text err, br_ []
        , img_ [src_ . imageDataUrl $ img]
        ]
    stageView (ResultStage result img)
      = eanView result ++
        [ img_ [src_ . imageDataUrl $ img] ]

    eanView (Good (EAN13 ean13 err))
      = [ text . ms $ "Barcode: " ++ show ean13, br_ []
        , text . ms $ "Error: "   ++ show err, br_ []
        ]
    eanView (Bad  (EAN13 ean13 err))
      = [ text        "ERROR TOO LARGE, barcode may be innacurate", br_ []
        , text . ms $ "Barcode: " ++ show ean13, br_ []
        , text . ms $ "Error: "   ++ show err, br_ []
        ]

foreign import javascript unsafe "$r = new FileReader();"
  newReader :: IO JSVal

foreign import javascript unsafe "$r = $1.files[0];"
  getFile :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.onload = $2;"
  setOnLoad :: JSVal -> Callback (IO ()) -> IO ()

foreign import javascript unsafe "$r = $1.result;"
  getResult :: JSVal -> IO MisoString

foreign import javascript unsafe "$1.readAsDataURL($2);"
  readDataURL :: JSVal -> JSVal -> IO ()

getSize :: JSVal -> IO (Maybe Int)
getSize v
  | isUndefined v = return Nothing
  | otherwise     = Just <$> getSize0 v

foreign import javascript unsafe "$r = $1.size;"
  getSize0 :: JSVal -> IO Int
