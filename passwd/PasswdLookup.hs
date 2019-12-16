module PasswdLookup where

import qualified Data.Map      as M
import           System.IO     (readFile, FilePath)
import           Text.Printf   (printf)
import           Text.Read     (lexP, readPrec, ReadPrec)
import           Text.Read.Lex (Lexeme(Ident, Symbol))
import           Text.ParserCombinators.ReadPrec
                 (get, look)

data PasswdEntry = PasswdEntry {
  userName     :: String,
  uid          :: Int,
  gid          :: Int,
  readableName :: String,
  homeDir      :: FilePath,
  shell        :: FilePath
  } deriving (Eq, Ord)

instance Show PasswdEntry where
  show e = printf "%s:x:%i:%i:%s:%s:%s"
           (userName e) (uid e) (gid e)
           (readableName e) (homeDir e) (shell e)

readPasswdString :: ReadPrec String
readPasswdString = do
  s <- look
  case s of
    ""    -> return ""
    ':':_ -> get >> return ""
    _     -> do
      c <- get
      chars <- readPasswdString
      return $ c:chars

readPasswdNum :: ReadPrec Int
readPasswdNum = do
  s <- readPasswdString
  return $ read s

readPasswdEntry0 :: ReadPrec (String, Int, Int, String)
readPasswdEntry0 = do
  userName     <- readPasswdString
  "x"   <- readPasswdString
  uid          <- readPasswdNum
  gid          <- readPasswdNum
  readableName <- readPasswdString
  -- homeDir      <- readPasswdString
  -- shell        <- readPasswdString
  return (userName, uid, gid, readableName)

-- readPrec_to_S readPasswdEntry 0 "ilya:x:1000:1000:abc:/home/ilya:/bin/bash"
readPasswdEntry :: ReadPrec PasswdEntry
readPasswdEntry = do
  userName     <- readPasswdString
  "x"          <- readPasswdString
  uid          <- readPasswdNum
  gid          <- readPasswdNum
  readableName <- readPasswdString
  homeDir      <- readPasswdString
  shell        <- readPasswdString
  return $ PasswdEntry userName uid gid readableName homeDir shell

instance Read PasswdEntry where
  -- read "Right ilya:x:1000:1000:abc:/home/ilya:/bin/bash" :: Either String PasswdEntry
  readPrec = readPasswdEntry

readPasswdEntries :: String -> [PasswdEntry]
readPasswdEntries = map read . lines

type UIDMap      = M.Map Int    PasswdEntry
type UsernameMap = M.Map String PasswdEntry

passwdEntryMaps :: [PasswdEntry] -> (UIDMap, UsernameMap)
passwdEntryMaps = foldr updateMaps (M.empty, M.empty)
  where updateMaps p (uidMap, usernameMap) =
          (M.insert (uid p)      p uidMap,
           M.insert (userName p) p usernameMap)
   
