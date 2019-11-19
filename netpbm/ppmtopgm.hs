{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import PPM2PGM

import Options.Generic

data Args = Args {
  input  :: FilePath,
  output :: FilePath
  } deriving (Generic, Show)

instance ParseRecord Args

main = do
  a <- getRecord ""
  convertPPMToPGM (input a) (output a)
