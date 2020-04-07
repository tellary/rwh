{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MonadHandle where

import Control.Monad.Catch (MonadMask)
import System.IO           (IOMode)

class (Monad m, MonadMask m) => MonadHandle h m | m -> h where
  hClose   :: h -> m ()
  hPutStr  :: h -> String -> m ()
  openFile :: FilePath -> IOMode -> m h

