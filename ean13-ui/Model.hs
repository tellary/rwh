{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module Model where

import Control.Concurrent (ThreadId)
import Control.Lens       (makeLenses)
#ifdef __GHCJS__
import Miso.String        (MisoString)
#else
type MisoString = String
#endif

newtype ImageString
  = ImageString { imageStr :: MisoString } deriving (Eq, Show)

data Model
  = Model
  { imageUrl :: MisoString
  , _barcode  :: Either MisoString Barcode
  } deriving (Eq, Show)

data Barcode
  = Barcode
  { _threadId :: Loading ThreadId
  , _image    :: Loading ImageString
  , _result   :: Loading Result
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

$(makeLenses ''Model)
$(makeLenses ''Barcode)
