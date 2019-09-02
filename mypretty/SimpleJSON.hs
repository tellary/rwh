module SimpleJSON (
  JValue(..)
  ) where

data JValue = JNumber Double |
              JString String |
              JBool Bool |
              JNull |
              JArray [JValue] |
              JObject [(String, JValue)] deriving (Show)
            
