{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Miso                    (App (..), View, asyncCallback, br_, consoleLog,
                                defaultEvents, div_, getElementById, id_, img_,
                                input_, noEff, onChange, src_, startApp, text,
                                type_, (<#))
import Miso.String             (MisoString)

import GHCJS.Foreign.Callback  (Callback)
import GHCJS.Types             (JSVal)

data Model
  = Model
  { img :: Maybe MisoString
  } deriving (Eq, Show)

data Action
  = ReadFile
  | NoOp
  | SetContent MisoString

app = App { model         = Model Nothing
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
      putMVar mvar r
  readDataURL reader file
  SetContent <$> readMVar mvar
updateModel (SetContent c) m = noEff m { img = Just c }
updateModel NoOp m = noEff m

viewModel :: Model -> View Action
viewModel (Model img)
  = div_ [] $ [
    "Barcode recognition"
    , br_ []
    , input_ [ id_ "fileReader"
             , type_ "file"
             , onChange (const ReadFile)
             ]
    , br_ []
    ] ++ imgView img
  where
    imgView Nothing    = [text "No barcode image loaded"]
    imgView (Just img) = [img_ [src_ img]]

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
