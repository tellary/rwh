{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Exception       (SomeException, catch, displayException)
import GHCJS.Foreign.Callback  (Callback)
import GHCJS.Types             (JSVal)
import Helper                  (ean13, parseImage)
import Miso                    (App (..), View, accept_, asyncCallback,
                                batchEff, br_, consoleLog, defaultEvents, div_,
                                getElementById, id_, img_, input_, noEff,
                                onChange, src_, startApp, text, type_, (<#))
import Miso.String             (MisoString, fromMisoString, ms)
import Text.Printf             (printf)

newtype ImageString
  = ImageString { imageStr :: MisoString } deriving (Eq, Show)

data Model
  = Model
  { image  :: Loading ImageString
  , result :: Loading Result
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
  -- `ShowProgress` has two `MVar` to carry them out `ReadFile`
  -- action handler and "batch" them into separate events
  -- `ShowProgress` handler.
  | ShowProgress (MVar ImageString) (MVar Result)
  | SetImage  ImageString
  | SetResult Result

app = App { model         = Nothing
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
  file            <- getFile fileReaderInput
  reader          <- newReader
  imageMVar       <- newEmptyMVar
  resultMVar      <- newEmptyMVar
  setOnLoad reader =<< do
    asyncCallback $ do
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
  return $ ShowProgress imageMVar resultMVar
updateModel (ShowProgress imageMVar resultMVar) _
  = batchEff (Just (Model Loading Loading))
    [ SetImage  <$> readMVar imageMVar
    , SetResult <$> readMVar resultMVar
    ]
updateModel (SetImage  img) (Just m) = noEff (Just m { image  = Loaded img })
updateModel (SetResult r  ) (Just m) = noEff (Just m { result = Loaded r   })
updateModel NoOp m       = noEff m
updateModel _    Nothing = noEff Nothing

viewModel :: Maybe Model -> View Action
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
    imgView Nothing =
      [ text "No barcode image choosen" ]
    imgView (Just (Model Loading      Loading)) =
      [ text "Loading image ..." ]
    imgView (Just (Model (Loaded img) Loading)) =
      [ text "Recognizing EAN13 barcode ...", br_ []
      , img_ [src_ . imageStr $ img]
      ]
    imgView (Just (Model Loading      (Loaded result))) =
      eanView result ++
      [ text "Image is still loading (weird) ..." ]
    imgView (Just (Model (Loaded img) (Loaded result))) =
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
