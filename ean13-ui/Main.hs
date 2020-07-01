{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import           Control.Concurrent            (ThreadId, killThread)
import           Control.Concurrent.Async      (async, asyncThreadId, wait)
import           Control.Exception             (Handler (Handler), catches,
                                                throw)
import           Control.Monad                 (when)
import           Data.Bifunctor                (bimap)
import           Data.List                     (intercalate)

import           Codec.Picture                 (Image, PixelRGB8)
import           Control.Concurrent.MVar       (newEmptyMVar, putMVar, readMVar)
import           Control.Exception             (SomeException, catch,
                                                displayException, handle)
import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as B (length)
import qualified Data.ByteString.Char8         as C (pack)
import           GHCJS.Foreign                 (isUndefined)
import           GHCJS.Foreign.Callback        (Callback)
import           GHCJS.Marshal                 (fromJSValUnchecked)
import           GHCJS.Types                   (JSString, JSVal)
import           Helper                        (createDataUrl, ean13,
                                                errorCutoff,
                                                parseImageByteString,
                                                parseImageDataUrl)
import           JavaScript.Web.XMLHttpRequest (Method (GET), Request (..),
                                                RequestData (NoData),
                                                Response (contents), XHRError,
                                                xhrByteString)
import           Miso                          (App (..), Effect, View, a_,
                                                accept_, asyncCallback,
                                                asyncCallback1, b_, br_,
                                                button_, consoleLog,
                                                defaultEvents, div_,
                                                getElementById, href_, id_,
                                                img_, input_, noEff, onChange,
                                                onClick, placeholder_, src_,
                                                startApp, text, type_, (<#))
import           Miso.String                   (MisoString, append,
                                                fromMisoString, ms, null,
                                                toMisoString)
import           Model                         (BarcodeStage (..), EAN13 (..),
                                                GalleryItem (GalleryItem,
                                                             itemDesc, itemUrl),
                                                ImageDataUrl (ImageDataUrl,
                                                              imageDataUrl),
                                                JobId,
                                                Model (Model, imageUrl, jobId,
                                                       stage, threadId),
                                                Result (Bad, Good),
                                                UIException (UIException))
import           Prelude                       hiding (null)
import           Text.Printf                   (printf)

data Action
  = NoOp
  | UpdateImageUrl MisoString
  | FetchImageAction
  | StartJob JobAction
  | ContinueJob JobId JobAction

data JobAction
  = SetThread ThreadId (IO JobAction)
  | ReadImage
  | FetchImage MisoString
  | SetDataUrl ImageDataUrl
  | SetImageBytes ByteString
  | SetImage (Image PixelRGB8) ImageDataUrl
  | SetError MisoString
  | SetResult Result

noBarcodeChosen = "No barcode image chosen"
initModel
  = Model 0 "" Nothing (ErrorStage $ ms noBarcodeChosen)

goodGallery
  = [ GalleryItem
      "https://ean13-samples.s3-us-west-2.amazonaws.com/ean13_1.jpg"
      "[1,2,3,4,5,6,7,8,9,0,1,2,8].jpg"
    , GalleryItem
      "https://ean13-samples.s3-us-west-2.amazonaws.com/ean13_3.jpg"
      "[0,7,0,5,6,3,2,0,8,5,9,4,3].jpg"
    , GalleryItem
      "https://ean13-samples.s3-us-west-2.amazonaws.com/ean13_8.bmp"
      "[0,1,2,3,4,5,6,7,8,9,1,0,4].bmp"
    , GalleryItem
      "https://ean13-samples.s3-us-west-2.amazonaws.com/ean13_9.gif"
      "[0,7,0,5,6,3,2,4,4,1,9,4,7].gif"
    , GalleryItem
      "https://ean13-samples.s3-us-west-2.amazonaws.com/ean13_4.jpg"
      "[3,8,0,0,0,6,5,7,1,1,1,3,5].jpg"
    , GalleryItem
      "https://ean13-samples.s3-us-west-2.amazonaws.com/ean13_5.jpg"
      "[4,9,0,2,5,0,6,3,0,4,9,1,9].jpg (Color photo)"
    , GalleryItem
      "https://ean13-samples.s3-us-west-2.amazonaws.com/ean13_6.jpeg"
      "[0,0,7,2,5,1,2,0,4,4,9,0,2].jpg (Photo, Made in Japan)"
    , GalleryItem
      "https://ean13-samples.s3-us-west-2.amazonaws.com/ean13_7.jpeg"
      "[8,7,1,8,8,6,8,6,6,9,0,7,0].jpg (Photo taken from a side)"
    ]

badGallery
  = [ GalleryItem
      "https://ean13-samples.s3-us-west-2.amazonaws.com/ean13_2_bad.jpeg"
      "Barcode that's not recognized by any tool online"
    , GalleryItem
      "https://ean13-samples.s3-us-west-2.amazonaws.com/zebra.jpeg"
      "Zebra picture"
    , GalleryItem
      "https://ean13-samples.s3-us-west-2.amazonaws.com/zebras_bad.jpeg"
      "Many zebras"
    , GalleryItem
      "https://ean13-samples.s3-us-west-2.amazonaws.com/book.jpeg"
      "Book"
    , GalleryItem
      "https://ean13-samples.s3-us-west-2.amazonaws.com/ean13.png"
      "PNG barcode (PNG not supported)"
    , GalleryItem
      "https://ean13-samples.s3-us-west-2.amazonaws.com/lion.jpeg"
      "Lion"
    , GalleryItem
      "https://ean13-samples.s3-us-west-2.amazonaws.com/racoon.jpeg"
      "Racoon"
    ]

app = App { model         = initModel
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
  exLog ("fetch image at " ++ show url) $ do
    when (null url) $ throw . UIException $ "Can't fetch an empty URL"
    handle xhrError (contents <$> xhrByteString req) >>= \case
      Nothing -> throw . UIException
                 $ printf "Failed to fetch image at %s. Nothing returned" url
      Just bs -> return bs
  where
    xhrError (_ :: XHRError) =
      throw . UIException
      $ printf
        "Failed to fetch image at %s. \
        \This is most likely caused by the HTTP request being \
        \blocked due to lack of CORS policy headers \
        \in the response. Try to download this URL and read the file \
        \from you computer by choosing it."
        url
    req = Request { reqMethod          = GET
                  , reqURI             = fromMisoString $ url
                  , reqLogin           = Nothing
                  , reqHeaders         = []
                  , reqWithCredentials = False
                  , reqData            = NoData
                  }

sizeLimit :: Int
sizeLimit  = 1024*sizeLimitK
sizeLimitK = 256

checkSize :: Int -> IO ()
checkSize size = do
  consoleLog . ms $ "File size: " ++ show size
  when (size > sizeLimit)
    . throw . UIException
    $ printf "Image file is too big, %ik max. It's %ik"
      sizeLimitK sizeK
  where sizeK = size `div` 1024

exLog :: String -> IO a -> IO a
exLog msg a
  = a `catches`
    [ Handler $ \(e :: UIException  ) -> do throw e
    , Handler $ \(e :: SomeException) -> do
        let msg1 = "Failed to " ++ msg ++ ": " ++ displayException e
        consoleLog .  ms $ msg1
        throw . UIException $ msg1
    ]

exAction :: IO JobAction -> IO JobAction
exAction a
  = catch a $ \(e :: SomeException) -> do
      return . SetError . ms $ displayException e

readImageFromFile :: IO ImageDataUrl
readImageFromFile = do
  fileReaderInput <- getElementById "fileReader"
  file            <- getFile fileReaderInput
  getSize file >>= \case
    Nothing   -> throw . UIException $ noBarcodeChosen
    Just _ -> do
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
      $  printf "Image isn't of a supported type.\n\
                \Please note that .png isn't supported.\n\
                \The following types were tried:\n%s"
                (intercalate ",\n" (map showErr errs))
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


updateStage :: JobAction -> BarcodeStage -> Effect JobAction BarcodeStage
updateStage ReadImage _
  = ImageReadingStage <# do
      exAction $ do
        dataUrl <- readImageFromFile
        return . SetDataUrl $ dataUrl
updateStage (FetchImage url) _
  = ImageFetchingStage <# do
      job <- async . fetchImage $ url
      return . SetThread (asyncThreadId job) . exAction $ do
        bs <- wait job
        checkSize (B.length bs)
        return . SetImageBytes $  bs
updateStage (SetDataUrl dataUrl) _
  = ImageDecodingStage (Just dataUrl) <# do
      job <- async $ do
        mvar <- newEmptyMVar
        resize (imageDataUrl dataUrl) =<< do
          asyncCallback1 $ \dataUrlJS -> do
            dataUrl1 <- ImageDataUrl . toMisoString
                        <$> (fromJSValUnchecked dataUrlJS :: IO JSString)
            consoleLog ("dataUrl1: " `append` imageDataUrl dataUrl1)
            img <- parseFileImage dataUrl1
            putMVar mvar (dataUrl1, img)
        readMVar mvar
      return . SetThread (asyncThreadId job) . exAction $ do
        (dataUrl1, img) <- wait job
        return $ SetImage img dataUrl1
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
updateModel (StartJob a) m
  = m { jobId = newJobId } <# do
      maybeKillJob m
      return $ ContinueJob newJobId a
  where newJobId = jobId m + 1
-- We check if `actionJobId` matches what's in the model,
-- the action is ignored if not.
--
-- We try to kill an existing
-- job's thread, but it's not always possible as
-- `SetThread` for a next job's stage
-- may be processed after we start a new job.
-- We create a new `JobId` each time we start a new job to
-- handle this case.
-- If an existing job's thread isn't killed when starting a new
-- job then the `actionJobId` is different from what's in the model and
-- the event is ignored.
updateModel (ContinueJob actionJobId a) m
  | jobId m == actionJobId
  = case a of
      SetThread thId act -> m { threadId = Just thId } <# (toAct <$> act)
      _                  -> bimap toAct toModel $ updateStage a (stage m)
  | otherwise
  = m <# return NoOp
  where toModel s = m { stage = s }
        toAct a = ContinueJob actionJobId a
updateModel (UpdateImageUrl url) m
  = m { imageUrl = url } <# do
      consoleLog ("url: " `append` url)
      return NoOp
updateModel (FetchImageAction) m
  = m <# (return . StartJob . FetchImage . imageUrl $ m)
updateModel NoOp m = noEff m

viewModel :: Model -> View Action
viewModel m
  = div_ [] $ [
    b_ [] [ text "Barcode recognition written in Haskell and \
                 \running in your browser" ]
    , br_ [], br_ []
    , text "Choose a barcode image from your file system", br_ []
    , input_ [ id_ "fileReader"
             , type_ "file"
             , accept_ "image/*"
             , onChange (const (StartJob ReadImage))
             ]
    , text ", or", br_ [], br_ []
    , text "Fetch a barcode image by providing an URL below", br_ []
    , text "(please note that .png images are not supported and an image \
           \may be blocked by a CORS policy of the image's host)", br_ []
    , input_ [ placeholder_ "Barcode image URL"
             , type_ "url"
             , onChange UpdateImageUrl
             ]
    , button_ [ onClick FetchImageAction ] [ text "Fetch barcode"]
    , text ", or", br_ [], br_ []
    , text "Pick a sample by clicking a link from the gallery below."
    ] ++ modelView m
  where
    modelView m
      =  galleryView
      ++ [br_ [], br_ []]
      ++ (resultView . stage $ m)

    resultView s
      =  [ b_ [] [ text "Barcode recognition result" ], br_ [] ]
      ++ stageView s

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
        , text . ms $ "Recognition error: "   ++ show err, br_ []
        , text        "(The lower error is the better. 0 is the ideal)", br_ []
        ]
    eanView (Bad  (EAN13 ean13 err))
      =  [ text        "ERROR TOO LARGE, barcode may be innacurate", br_ [] ]
      ++ eanView (Good (EAN13 ean13 err))

    galleryView
      =  [br_ [], br_ []
         , b_ [] [ text "Samples gallery" ]
         , br_ []
         ]
      ++ concat (map itemView goodGallery)
      ++ [ br_ [],
           text "Negative samples:", br_ [] ]
      ++ concat (map itemView badGallery)

    itemView i
      = [ a_ [ href_ . ("#" `append`) . itemDesc $ i
             , onClick . StartJob . FetchImage . itemUrl $ i
             ]
             [ text . itemDesc $ i]
        , br_ []
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

-- See resize.js
foreign import javascript unsafe "ean13_resize($1, $2)"
  resize :: MisoString -> Callback (JSVal -> IO ()) -> IO ()
