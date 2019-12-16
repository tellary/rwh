import PasswdLookup
import Control.Exception (assert)
import Text.ParserCombinators.ReadPrec

v1 = read "ilya:x:1000:1000:abc:/home/ilya:/bin/bash"
t1 = assert (userName v1 == "ilya") "t1"

v2 = read "systemd-timesync:x:103:104:systemd Time Synchronization,,,:/run/systemd:/bin/false"
t2 = assert (userName v2 == "systemd-timesync") "t2"

v3 = readPasswdEntries <$> readFile "./passwd"
t3 = flip assert "All ./passwd entries have userName"
     . and . map (not . null . userName) <$> v3

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

tests :: IO [String]
tests = do
  t3' <- t3
  return [t1, t2, t3', t4, t5]
