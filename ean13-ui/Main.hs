{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Concurrent      (ThreadId, killThread, myThreadId)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Exception       (SomeException, catch, displayException)
import Control.Lens            ((&), (.~), (^.), (^?), _Right)
import GHCJS.Foreign           (isUndefined)
import GHCJS.Foreign.Callback  (Callback)
import GHCJS.Types             (JSVal)
import Helper                  (ean13, errorCutoff, parseImage)
import Miso                    (App (..), Effect, View, accept_, asyncCallback,
                                batchEff, br_, button_, consoleLog,
                                defaultEvents, div_, getElementById, id_, img_,
                                input_, noEff, onChange, onClick, placeholder_,
                                src_, startApp, text, type_, (<#))
import Miso.String             (MisoString, append, fromMisoString, ms)
import Text.Printf             (printf)
--import           JavaScript.Web.XMLHttpRequest (Request(..), xhrByteString)
import Model                   (Barcode (..), EAN13 (..),
                                ImageString (ImageString, imageStr),
                                Loading (Loaded, Loading),
                                Model (Model, imageUrl),
                                Result (Bad, Error, Good), barcode, image,
                                result, threadId)

data Action
  = ReadFile
  | UpdateImageURL MisoString
  | FetchImage
  | NoOp
  -- `ShowProgress` has several `MVar`s to carry them out of the `ReadFile`
  -- handler and "batch" them into separate events in the
  -- `ShowProgress` handler.
  | ShowProgress (MVar ThreadId) (MVar ImageString) (MVar Result)
  | SetThread ThreadId
  | SetImage  ImageString
  | SetResult Result
  | SetError  MisoString

noBarcodeChosen = "No barcode image chosen"
app = App { model         = Model "" (Left noBarcodeChosen)
          , initialAction = NoOp
          , update        = updateModel
          , view          = viewModel
          , events        = defaultEvents
          , subs          = []
          , mountPoint    = Nothing
          }

main = startApp app

readBarcode :: Model -> JSVal -> IO Action
readBarcode m file = do
  reader          <- newReader
  imageMVar       <- newEmptyMVar
  resultMVar      <- newEmptyMVar
  threadMVar      <- newEmptyMVar
  case m ^? barcode . _Right . threadId of
    -- We keep track of a thread that does heavy lifting and
    -- try to kill it before starting next barcode recognition job.
    -- This takes care of a scenario when a long running job is started,
    -- then we choose a different image that's processed faster and
    -- a result of the faster processing is later overridden when the
    -- long running job completes.
    Just (Loaded tid) -> do
      consoleLog . ms $ "Killing existing job " ++ show tid
      killThread tid
    _ -> return ()
  setOnLoad reader =<< do
    asyncCallback $ do
      thId <- myThreadId
      putMVar threadMVar thId
      consoleLog . ms $ "myThreadId: " ++ show thId
      r <- ImageString <$> getResult reader
      putMVar imageMVar r
      catch
        (do
          case parseImage . fromMisoString . imageStr $ r of
            Right img ->
              case ean13 img of
                Right (err :: Double, ean13 :: [Int]) -> do
                  consoleLog . ms @String
                    $ printf "Found EAN13: %s\nError: %f" (show ean13) err
                  if err < errorCutoff
                    then putMVar resultMVar . Good $ EAN13 ean13 err
                    else putMVar resultMVar . Bad  $ EAN13 ean13 err
                Left err ->
                  putMVar resultMVar . Error . ms $ err
            Left err -> putMVar resultMVar . Error . ms $ err)
        (\(e :: SomeException) -> do
            let msg = "Failed to load image or process EAN13: "
                      ++ displayException e
            consoleLog . ms $ msg
            putMVar resultMVar . Error . ms $ msg
        )
  readDataURL reader file
  return $ ShowProgress threadMVar imageMVar resultMVar

sizeLimit :: Int
sizeLimit = 1024*256

updateModel :: Action
            -> Model
            -> Effect Action (Model)
updateModel ReadFile m = m <# do
  fileReaderInput <- getElementById "fileReader"
  file            <- getFile fileReaderInput
  s               <- getSize file
  case s of
    Nothing   -> return . SetError $ noBarcodeChosen
    Just size -> do
      consoleLog . ms $ "File size: " ++ show size
      if size > sizeLimit
        then return . SetError . ms
             $ "Image file is too big, 512k max. It's "
             ++ show (size `div` 1024) ++ "k."
        else readBarcode m file
updateModel FetchImage m = m <# do
  consoleLog . ms $ "Fetching image " ++ show (imageUrl m)
  return NoOp
updateModel (UpdateImageURL url) m
  = m { imageUrl = url } <# do
      consoleLog ("url: " `append` url)
      return NoOp
updateModel (SetError err) m = noEff $ m & barcode .~ Left err
updateModel (ShowProgress threadMVar imageMVar resultMVar) m
  = batchEff m1 as
  where c  = barcode . _Right
        m1 = case m ^. barcode of
               Left  _ -> m & barcode
                            .~ Right (Barcode Loading Loading Loading)
               Right _ -> m & c . image  .~ Loading
                            & c . result .~ Loading
        as = [ SetThread <$> readMVar threadMVar
             , SetImage  <$> readMVar imageMVar
             , SetResult <$> readMVar resultMVar
             ]
updateModel (SetThread tid) m
  = noEff $ m & barcode . _Right . threadId .~ Loaded tid
updateModel (SetImage  img) m
  = noEff $ m & barcode . _Right . image    .~ Loaded img
updateModel (SetResult r) m
  = noEff $ m & barcode . _Right . result   .~ Loaded r
updateModel NoOp m          = noEff m

viewModel :: Model -> View Action
viewModel m
  = div_ [] $ [
    "Barcode recognition"
    , br_ []
    , input_ [ id_ "fileReader"
             , type_ "file"
             , accept_ "image/*"
             , onChange (const ReadFile)
             ]
    , br_ []
    , input_ [ placeholder_ "Barcode image URL"
             , type_ "url"
             , onChange UpdateImageURL
             ]
    , button_ [ onClick FetchImage ] [ text "Read barcode"]
    , br_ []
    ] ++ modelView m
  where
    modelView (Model _ (Left err)) = [ text err ]
    modelView (Model _ (Right j))  = jobView j

    jobView (Barcode _ Loading      Loading)
      = [ text "Loading image ..." ]
    jobView (Barcode _ (Loaded img) Loading)
      = [ text "Recognizing EAN13 barcode ...", br_ []
        , img_ [src_ . imageStr $ img]
        ]
    jobView (Barcode _ Loading      (Loaded result))
      = eanView result ++
        [ text "Image is still loading (weird) ..." ]
    jobView (Barcode _ (Loaded img) (Loaded result))
      = eanView result ++
        [ img_ [src_ . imageStr $ img] ]

    eanView (Error err)
      = [ text . ms $ err, br_ [] ]
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
