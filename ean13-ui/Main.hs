{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Exception       (SomeException, catch, displayException)
import Helper                  (ean13, parseImage)
import Miso                    (App (..), View, accept_, asyncCallback, br_,
                                consoleLog, defaultEvents, div_, getElementById,
                                id_, img_, input_, noEff, onChange, src_,
                                startApp, text, type_, (<#))
import Miso.String             (MisoString, fromMisoString, ms)
import GHCJS.Foreign.Callback  (Callback)
import GHCJS.Types             (JSVal)
import Text.Printf             (printf)

newtype ImageString = ImageString { imageStr :: MisoString } deriving (Eq, Show)

data Model
  = ErrorModel           MisoString
  | NoBarcodeModel       NoBarcode
  | ResultModel          Result
  | HighErrorResultModel Result
  deriving (Eq, Show)

data NoBarcode
  = NoBarcode
  { noBarcodeImage :: ImageString
  , noBarcodeError :: MisoString
  } deriving (Eq, Show)

data Result
  = Result
  { resultImage :: ImageString
  , resultEan13 :: [Int]
  , resultError :: Double
  } deriving (Eq, Show)

data Action
  = ReadFile
  | NoOp
  | SetContent Model

app = App { model         = ErrorModel "No barcode image loaded"
          , initialAction = NoOp
          , update        = updateModel
          , view          = viewModel
          , events        = defaultEvents
          , subs          = []
          , mountPoint    = Nothing
          }

main = startApp app

updateModel ReadFile m = m <# do
  consoleLog "ReadFile"
  fileReaderInput <- getElementById "fileReader"
  file <- getFile fileReaderInput
  reader <- newReader
  mvar <- newEmptyMVar
  setOnLoad reader =<< do
    asyncCallback $ do
      r <- ImageString <$> getResult reader
      catch
        (do
          case parseImage . fromMisoString . imageStr $ r of
            Right img ->
              case ean13 img of
                Right (err :: Double, ean13 :: [Int]) -> do
                  consoleLog . ms @String
                    $ printf "Found EAN13: %s\nError: %f" (show ean13) err
                  if err < 0.15 -- 14.25 extra lines in a barcode of 95 lines
                    then putMVar mvar . ResultModel $ Result r ean13 err
                    else putMVar mvar
                         . HighErrorResultModel $ Result r ean13 err
                Left err ->
                  putMVar mvar . NoBarcodeModel $ NoBarcode r (ms $ err)
            Left err ->
              putMVar mvar . ErrorModel . ms $ err)
        (\(e :: SomeException) -> do
            let msg = "Failed to load image or process EAN13: "
                      ++ displayException e
            consoleLog . ms $ msg
            putMVar mvar . ErrorModel . ms $ msg
        )
  readDataURL reader file
  SetContent <$> readMVar mvar
updateModel (SetContent m) _ = noEff m
updateModel NoOp m = noEff m

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
    ] ++ imgView m
  where
    imgView (ErrorModel err) = [text err]
    imgView (NoBarcodeModel (NoBarcode img err)) =
      [ text err, br_ []
      , img_ [src_ . imageStr $ img]
      ]
    imgView (ResultModel (Result img ean13 err)) =
      [ text . ms $ "Barcode: " ++ show ean13, br_ []
      , text . ms $ "Error: "   ++ show err, br_ []
      , img_ [src_ . imageStr $ img]
      ]
    imgView (HighErrorResultModel (Result img ean13 err)) =
      [ text        "ERROR TOO LARGE, barcode may be innacurate", br_ []
      , text . ms $ "Barcode: " ++ show ean13, br_ []
      , text . ms $ "Error: "   ++ show err, br_ []
      , img_ [src_ . imageStr $ img]
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
