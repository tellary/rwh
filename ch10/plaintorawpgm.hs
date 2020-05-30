{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


import PGM

import Options.Generic

data Args = Args {
  input  :: FilePath,
  output :: FilePath
  } deriving (Generic, Show)

instance ParseRecord Args

main = do
  a <- getRecord ""
  convertPlainToRawPGM (input a) (output a)
