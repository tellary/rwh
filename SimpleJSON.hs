module SimpleJSON (
  JValue(..)
  ) where

data JValue = JNumber Double |
              JString String |
              JNull |
              JArray [JValue] |
              JObject [(String, JValue)] deriving (Show)
            
