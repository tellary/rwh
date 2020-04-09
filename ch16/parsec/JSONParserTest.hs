import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Control.Exception (assert)
import JSON
import JSONParser
import SimpleJSON
import Text.ParserCombinators.Parsec

s1 = fromRight JNull <$> parseFromFile jsonFile "s1.json"

lookupOrFail k = fromRight (error $ "Can't get " ++ k) . jobjLookup k

s1Name    :: IO String
s1Name    =  lookupOrFail "name" <$> s1
s1Address :: IO JValue
s1Address = lookupOrFail "address" <$> s1
s1Age     :: IO Double
s1Age     = lookupOrFail "age" <$> s1

father :: IO JValue
father = lookupOrFail "father" <$> s1

fatherName :: IO String
fatherName = lookupOrFail "name" <$> father

kids :: IO [JValue]
kids = fromJAry . fromRight (JAry []) . fromJValue
  . lookupOrFail "kids" <$> father

kid1 = head <$> kids

kid1Name :: IO String
kid1Name = lookupOrFail "name" <$> kid1

testsIO = [
  assert <$> (/= JNull) <$> s1         <*> pure "s1 parsed",
  assert <$> (== "Bob") <$> s1Name     <*> pure "Name is Bob",
  assert <$> (== JNull) <$> s1Address  <*> pure "Address is null",
  assert <$> (== -5)    <$> s1Age      <*> pure "Born 5 years in the future",
  assert <$> (== "Rob") <$> fatherName <*> pure "Father name is Rob",
  assert <$> (== 2)     <$> length <$> kids <*> pure "There are two kids",
  assert <$> (== "Bob") <$> kid1Name   <*> pure "Name is Bob"
  ]

tests = sequence testsIO
