{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import EAN13(readEAN13File0)
import Data.List(intercalate)
import Options.Generic

data Args = Full {
  rows            :: Maybe Int,
  candidateDigits :: Maybe Int,
  threshold       :: Maybe Double,
  input           :: FilePath }
  deriving (Generic, Show)

instance ParseRecord Args

main = do
  a           <- getRecord ""
  let r = maybe 3   id $ rows a
  let n = maybe 3   id $ candidateDigits a
  let t = maybe 0.4 id $ threshold a
  result <- readEAN13File0 r n t $ input a
  case result of
    Right (err, ean13) -> do
      putStrLn $ "EAN13: " ++ (intercalate "," $ map show ean13)
      putStrLn $ "Error: " ++ show err
    Left  e -> putStrLn e
