import Control.Exception (assert)
import PasswdLookup
import Text.ParserCombinators.ReadPrec

ilyaRecord = read "ilya:x:1000:1000::/home/ilya:/bin/bash"
t1 = assert (userName ilyaRecord == "ilya") "Name in `ilya` record is correct"

v2 = read "systemd-timesync:x:103:104:systemd Time Synchronization,,,:/run/systemd:/bin/false"
t2 = assert (userName v2 == "systemd-timesync") "t2"

passwdEntries = readPasswdEntries <$> readFile "./passwd"
t3 = flip assert "All ./passwd entries have userName"
     . and . map (not . null . userName) <$> passwdEntries

v4 = read "_apt:x:100:65534::/nonexistent:/bin/false"
t4 = assert (userName v4 == "_apt") "t4"

aptEntry = PasswdEntry {
  userName = "_apt",
  uid = 100,
  gid = 65534,
  readableName = "",
  homeDir = "/nonexistent",
  shell = "/bin/false"
  }

v5 = readPrec_to_S readPasswdEntry 0 "_apt:x:100:65534::/nonexistent:/bin/false"
t5 = assert (v5 == [(aptEntry,"")]) "t5"

maps = passwdEntryMaps <$> passwdEntries
t6 = flip assert "Record for uid 1000 is correct" .
     (== Just ilyaRecord) . flip lookupByUID 1000 <$> maps
t7 = flip assert "Record for name 'ilya' is correct" .
     (== Just ilyaRecord) . flip lookupByName "ilya" <$> maps


tests :: IO [String]
tests = do
  t3' <- t3
  t6' <- t6
  t7' <- t7
  return [t1, t2, t3', t4, t5, t6', t7']
