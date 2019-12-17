module PasswdLookup where

import qualified Data.Map      as M
import           Control.Monad (forM_)
import           System.Environment
                 (getArgs)
import           System.Exit   (exitFailure, exitSuccess)
import           System.IO     (readFile, FilePath)
import           Text.Printf   (printf)
import           Text.Read     (readPrec, ReadPrec)
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

type PasswdEntryMaps = (UIDMap, UsernameMap)

passwdEntryMaps :: [PasswdEntry] -> PasswdEntryMaps
passwdEntryMaps = foldr updateMaps (M.empty, M.empty)
  where updateMaps p (uidMap, usernameMap) =
          (M.insert (uid p)      p uidMap,
           M.insert (userName p) p usernameMap)

data MenuOption = MenuOption {
  menuName   :: String,
  menuDesc   :: String,
  menuAction :: PasswdEntryMaps -> IO ()
  }

instance Show MenuOption where
  show = menuName

lookupByName maps user = M.lookup user . snd $ maps
lookupByUID  maps u    = M.lookup u    . fst $ maps

lookupByNameOpt =
  MenuOption "Look up by username" "Looks for a passwd entry by a username"
  $ \maps -> do
      putStrLn "Enter user name:"
      user <- getLine
      case lookupByName maps user of
        Just e  -> putStrLn $ "Found entry: " ++ show e
        Nothing -> putStrLn "No entry"

lookupByUIDOpt =
  MenuOption "Look up by UID" "Looks for a passwd entry by UID"
  $ \maps -> do
      putStrLn "Enter UID:"
      uidStr <- getLine
      case reads uidStr of
        [(uidNum, _)] ->
          case lookupByUID maps uidNum of
            Just e  -> putStrLn $ "Found entry: " ++ show e
            Nothing -> putStrLn "No entry"
        _             -> printf "'%s' is not a number\n" uidStr

showPasswdOpt =
  MenuOption "Show passwd" "Shows entire passwd file"
  $ \maps -> mapM_ (putStrLn . show . snd) $ M.assocs $ fst maps

showHelpOpt =
  MenuOption "Help" "Shows help on all menu options"
  $ \_ -> do
  forM_ menuOpts $ \(n, m) -> printf "%i. %s\n" n $ menuDesc m
  putStrLn ""

listAllUsernamesOpt =
  MenuOption "List users" "Lists all user names"
  $ \maps -> do
  mapM_ putStrLn . M.keys . snd $ maps

exitOpt =
  MenuOption "Exit" "Exits the program"
  $ \_ -> exitSuccess

menuOpts = zip [1..]
           [lookupByNameOpt,
            lookupByUIDOpt,
            listAllUsernamesOpt,
            showPasswdOpt,
            showHelpOpt,
            exitOpt]
menuMap :: M.Map Int MenuOption
menuMap = M.fromList menuOpts

passwdLookupMenu1 maps = do
  putStrLn "-----------"
  passwdLookupMenu maps

passwdLookupMenu :: PasswdEntryMaps -> IO ()
passwdLookupMenu maps = do
  mapM (\(n, m) -> printf "%i. %s\n" n $ show m) menuOpts
  itemNumberStr <- getLine
  case reads itemNumberStr of
    [(itemNumber, _)] -> do
      case M.lookup itemNumber menuMap of
        Just opt -> menuAction opt maps >> passwdLookupMenu1 maps
        Nothing  -> do
          printf "No menu item %i\n" itemNumber
          passwdLookupMenu1 maps
    _                 -> do
      printf "'%s' is not a number\n" itemNumberStr
      passwdLookupMenu1 maps

passwdLookupMain :: String -> IO ()
passwdLookupMain pn = do
  args <- getArgs
  case args of
    [fileName] -> do
      passwdEntries <- readPasswdEntries <$> readFile fileName
      passwdLookupMenu $ passwdEntryMaps passwdEntries
    _          -> do
      putStrLn $ printf "Usage: %s passwdFile" pn
      exitFailure
