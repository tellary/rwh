{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module Model where

import Control.Exception  (Exception)
import Control.Concurrent (ThreadId)
#ifdef __GHCJS__
import Miso.String        (MisoString)
#else
type MisoString = String
#endif

newtype ImageDataUrl
  = ImageDataUrl { imageDataUrl :: MisoString } deriving (Eq, Show)

type JobId = Int

data Model
  = Model
  { jobId    :: JobId
  , imageUrl :: MisoString
  , threadId :: Maybe ThreadId
  , stage    :: BarcodeStage
  } deriving (Eq, Show)

data BarcodeStage
  = ImageReadingStage
  | ImageFetchingStage
  | ImageDecodingStage (Maybe ImageDataUrl)
  | BarcodeRecognitionStage ImageDataUrl
  | ErrorStage MisoString
  | ErrorImageStage MisoString ImageDataUrl
  | ResultStage Result ImageDataUrl
  deriving (Eq, Show)

data Result = Bad EAN13 | Good EAN13 deriving (Eq, Show)

data EAN13
  = EAN13
  { eanDigits :: [Int]
  , eanError  :: Double
  } deriving (Eq, Show)

data GalleryItem
  = GalleryItem
  { itemUrl   :: MisoString
  , itemDesc  :: MisoString
  } deriving (Eq, Show)

data UIException = UIException String

instance Show UIException where
  show (UIException m) = m
instance Exception UIException
