{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MonadHandle where

import System.IO (Handle, IOMode)

class Monad m => MonadHandle h m | m -> h where
  hClose   :: h -> m ()
  hPutStr  :: h -> String -> m ()
  openFile :: FilePath -> IOMode -> m h

