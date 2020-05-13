import Control.Exception
import PodDownload
import PodDB
import PodTypes
import System.IO.Error
import System.Directory

d = do
  removeFile "pod-test.db"
    `catch` \e ->
              if doesNotExistErrorType == ioeGetErrorType e
              then return ()
              else throw e
  conn <- openFK "pod-test.db"
  initDB conn
  let p0 = $$(podcast 666 "http://feed.thisamericanlife.org/talpodcast")
  p <- addPodcast conn p0
  downloadPodcastInfo conn p
  es <- listPodcastEpisodesByDone conn p False
  downloadEpisodes conn es
  
