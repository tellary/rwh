module PrettyJSON(renderJValue) where

import Prettifier
import SimpleJSON

enclose :: Char -> Char -> (a -> Doc) -> [a] -> Doc
enclose open close item = brace . punctuate . map item
  where brace d = char open <> indent </> d <> outdent </> char close

punctuate [] = empty
punctuate [d] = d
punctuate (d:ds) = d <> char ',' </> punctuate ds

renderJValue (JNumber d) = text $ show d
renderJValue (JString s) = char '"' <> text s <> char '"'
renderJValue JNull = text "null"
renderJValue (JArray a) = enclose '[' ']' renderJValue a
renderJValue (JObject o) = enclose '{' '}' obj o
  where obj (k, v) =
          renderJValue (JString k) <> text ": " <> renderJValue v

j1 = JArray [JNumber 1, JString "man", JString " & a ", JNull]
j2 = JObject [
  ("How", JString "many"),
  ("What?", JString "dogs"),
  ("Result", JNumber 1),
  ("Cats?", JNull)]
j3 = JObject [
  ("object", j2),
  ("array", JArray [j2, j1])
  ]
