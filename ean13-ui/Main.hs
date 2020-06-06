{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Concurrent      (ThreadId, killThread, myThreadId)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Exception       (SomeException, catch, displayException)
import GHCJS.Foreign           (isUndefined)
import GHCJS.Foreign.Callback  (Callback)
import GHCJS.Types             (JSVal)
import Helper                  (ean13, parseImage)
import Miso                    (App (..), Effect, View, accept_, asyncCallback,
                                batchEff, br_, consoleLog, defaultEvents, div_,
                                getElementById, id_, img_, input_, noEff,
                                onChange, src_, startApp, text, type_, (<#))
import Miso.String             (MisoString, fromMisoString, ms)
import Text.Printf             (printf)

newtype ImageString
  = ImageString { imageStr :: MisoString } deriving (Eq, Show)

data Model
  = Model
  { threadId :: Loading ThreadId
  , image    :: Loading ImageString
  , result   :: Loading Result
  } deriving (Eq, Show)

data Loading a
  = Loading
  | Loaded a
  deriving (Eq, Show)

data Result = Error MisoString | Bad EAN13 | Good EAN13 deriving (Eq, Show)

data EAN13
  = EAN13
  { eanDigits :: [Int]
  , eanError  :: Double
  } deriving (Eq, Show)

data Action
  = ReadFile
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
app = App { model         = Left noBarcodeChosen
          , initialAction = NoOp
          , update        = updateModel
          , view          = viewModel
          , events        = defaultEvents
          , subs          = []
          , mountPoint    = Nothing
          }

main = startApp app

readBarcode :: Either MisoString Model -> JSVal -> IO Action
readBarcode m file = do
  reader          <- newReader
  imageMVar       <- newEmptyMVar
  resultMVar      <- newEmptyMVar
  threadMVar      <- newEmptyMVar
  either (const $ return ())
    (\model ->
       -- We keep track of a thread that does heavy lifting and
       -- try to kill it before starting next barcode recognition job.
       -- This takes care of a scenario when a long running job is started,
       -- then we choose a different image that's processed faster and
       -- a result of the faster processing is later overridden when the
       -- long running job completes.
       case threadId model of
         Loaded tid -> do
           consoleLog . ms $ "Killing existing job " ++ show tid
           killThread tid
         _ -> return ()
    ) m
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
                  -- 14.25 extra lines in a barcode of 95 lines
                  if err < 0.15
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
            -> Either MisoString Model
            -> Effect Action (Either MisoString Model)
updateModel ReadFile m = m <# do
  fileReaderInput <- getElementById "fileReader"
  file            <- getFile fileReaderInput
  s               <- getSize file
  case s of
    Nothing   -> return . SetError $ noBarcodeChosen
    Just size -> do
      consoleLog . ms $ "File size: " ++ show s
      if size > sizeLimit
        then return . SetError . ms
             $ "Image file is too big, 512k max. It's "
             ++ show (size `div` 1024) ++ "k."
        else readBarcode m file
updateModel (SetError err) _ = noEff (Left err)
updateModel (ShowProgress threadMVar imageMVar resultMVar) (Left _)
  = batchEff (Right (Model Loading Loading Loading))
    [ SetThread <$> readMVar threadMVar
    , SetImage  <$> readMVar imageMVar
    , SetResult <$> readMVar resultMVar
    ]
updateModel (ShowProgress threadMVar imageMVar resultMVar) (Right m)
  = batchEff (Right m { image = Loading, result = Loading })
    [ SetThread <$> readMVar threadMVar
    , SetImage  <$> readMVar imageMVar
    , SetResult <$> readMVar resultMVar
    ]
updateModel (SetThread threadId) (Right m)
  = noEff (Right m { threadId = Loaded threadId })
updateModel (SetImage  img) (Right m) = noEff (Right m { image  = Loaded img })
updateModel (SetResult r  ) (Right m) = noEff (Right m { result = Loaded r   })
updateModel NoOp m          = noEff m
updateModel _    (Left err) = noEff (Left err)

viewModel :: Either MisoString Model -> View Action
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
    ] ++ imgView m
  where
    imgView (Left err) =
      [ text err ]
    imgView (Right (Model _ Loading      Loading)) =
      [ text "Loading image ..." ]
    imgView (Right (Model _ (Loaded img) Loading)) =
      [ text "Recognizing EAN13 barcode ...", br_ []
      , img_ [src_ . imageStr $ img]
      ]
    imgView (Right (Model _ Loading      (Loaded result))) =
      eanView result ++
      [ text "Image is still loading (weird) ..." ]
    imgView (Right (Model _ (Loaded img) (Loaded result))) =
      eanView result ++
      [ img_ [src_ . imageStr $ img] ]
    eanView (Error err) =
      [ text err, br_ [] ]
    eanView (Good (EAN13 ean13 err)) =
      [ text . ms $ "Barcode: " ++ show ean13, br_ []
      , text . ms $ "Error: "   ++ show err, br_ []
      ]
    eanView (Bad  (EAN13 ean13 err)) =
      [ text        "ERROR TOO LARGE, barcode may be innacurate", br_ []
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
