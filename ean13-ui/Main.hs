{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Codec.Picture           (imageHeight)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Exception       (SomeException, catch, displayException)
import Helper                  (parseImage)
import Miso                    (App (..), View, accept_, asyncCallback, br_,
                                consoleLog, defaultEvents, div_, getElementById,
                                id_, img_, input_, noEff, onChange, src_,
                                startApp, text, type_, (<#))
import Miso.String             (MisoString, fromMisoString, ms)

import GHCJS.Foreign.Callback  (Callback)
import GHCJS.Types             (JSVal)

data Model
  = Image MisoString
  | Error MisoString
  deriving (Eq, Show)

data Action
  = ReadFile
  | NoOp
  | SetContent Model

app = App { model         = Error "No barcode image loaded"
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
      r <- getResult reader
      catch
        (do
          let img = parseImage . fromMisoString $ r
          case img of
            Right img -> do
              consoleLog . ms $ "Loaded image height: "
                ++ (show . imageHeight $ img)
              putMVar mvar $ Image r
            Left err  -> do
              putMVar mvar . Error . ms $ err)
        (\(e :: SomeException) -> do
            let msg = "Failed to load image: " ++ displayException e
            consoleLog . ms $ msg
            putMVar mvar . Error . ms $ msg
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
    imgView (Error err) = [text err]
    imgView (Image img) = [img_ [src_ img]]

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
