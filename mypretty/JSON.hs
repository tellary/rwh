{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module JSON where

import SimpleJSON

type JSONError = String

class JSON a where
  fromJValue:: JValue -> Either JSONError a
  toJValue:: a -> JValue

instance JSON JValue where
  fromJValue = Right
  toJValue = id

instance JSON Bool where
  fromJValue (JBool b) = Right b
  fromJValue _ = Left "Not JBool"

  toJValue a = JBool a
  
instance JSON String where
  fromJValue (JString s) = Right s
  fromJValue _ = Left "Not JString"

  toJValue s = JString s

