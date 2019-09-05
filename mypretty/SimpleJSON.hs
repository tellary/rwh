module SimpleJSON (
  JValue(..), JAry(..), JObj(..)
  ) where

data JValue = JNumber Double |
              JString String |
              JBool Bool |
              JNull |
              JArray (JAry JValue) |
              JObject (JObj JValue) deriving (Show)
            
newtype JAry a = JAry {
  fromJAry :: [a]
  } deriving (Show)

newtype JObj a = JObj {
  fromJObj :: [(String, a)]
  } deriving (Show)

