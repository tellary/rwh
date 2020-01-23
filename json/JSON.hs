{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module JSON where

import Data.Either (fromRight)
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

instance JSON Double where
  fromJValue (JNumber a) = Right a
  fromJValue _ = Left "Not JNumber"

  toJValue d = JNumber d

jaryFromJValue :: (JSON a) => JValue -> Either JSONError (JAry a)
jaryFromJValue (JArray (JAry a)) =
  fmap JAry $ foldr step (Right []) (map fromJValue a)
jaryFromJValue _ = Left "Not a JArray"
-- v = [JString $ JAry "a", JNumber 1.0, JBool True]
-- fromJValue (JArray (JAry v)) :: Either JSONError (JAry JValue)
-- fromJValue (JArray (JAry [JString "a", JString "b"])) :: Either JSONError (JAry String)
-- fromJValue (JArray (JAry [JBool True, JBool False])) :: Either JSONError (JAry Bool)

step _ (Left err) = Left err
step (Left err) _ = Left err
step (Right x) (Right ys) = Right (x:ys)

jaryToJValue :: (JSON a) => JAry a -> JValue
jaryToJValue = JArray . JAry . map toJValue . fromJAry

instance (JSON a) => JSON (JAry a) where
  fromJValue = jaryFromJValue
  toJValue = jaryToJValue

jobjFromJValue :: (JSON a) => JValue -> Either JSONError (JObj a)
jobjFromJValue (JObject (JObj o)) =
  fmap JObj $ foldr step (Right []) (map (fmap fromJValue) o)
  where step (_, Left err) _ = Left err
        step (k, Right v) (Right r) = Right ((k,v):r)

jobjFromJValue _ = Left "Not a JObject"

-- jobjFromJValue $ JObject $ JObj [("1",JNumber 1.0),("2",JNumber 2.0)]
-- jobjFromJValue $ JObject $ JObj [("1",JBool True),("2",JBool False)] :: Either JSONError (JObj Bool)
-- jobjFromJValue $ JObject $ JObj [("1",JString "ab"),("2",JString "bc")] :: Either JSONError (JObj String)

jobjToJValue :: (JSON a) => JObj a -> JValue
jobjToJValue = JObject . JObj . map (fmap toJValue) . fromJObj
--  jobjToJValue $ JObj [("1", 1.0), ("2", 2)]

instance (JSON a) => JSON (JObj a) where
  fromJValue = jobjFromJValue
  toJValue = jobjToJValue

jobjAssoc :: JValue -> Either String [(String, JValue)]
jobjAssoc = fmap fromJObj . fromJValue

jobjLookup k = fromJValue' . lookup k . fromRight [] . jobjAssoc
  where fromJValue' Nothing = Left "Not found"
        fromJValue' (Just v)  = fromJValue v

