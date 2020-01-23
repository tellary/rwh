module SimpleJSON (
  JValue(..), JAry(..), JObj(..)
  ) where

data JValue = JNumber Double |
              JString String |
              JBool Bool |
              JNull |
              JArray (JAry JValue) |
              JObject (JObj JValue) deriving (Eq, Show)
            
newtype JAry a = JAry {
  fromJAry :: [a]
  } deriving (Eq, Show)

newtype JObj a = JObj {
  fromJObj :: [(String, a)]
  } deriving (Eq, Show)

