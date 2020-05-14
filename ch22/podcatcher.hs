import Data.Semigroup      ((<>))
import Options.Applicative (argument, command, idm, info, metavar, progDesc,
                            str)
import OptParseREPL        (repl)

data Command = Add String
             | Update
             | Download
             | Delete String
             | Quit
             deriving Show

cmds
  =  command "add"
     (info
       (Add <$> argument str (metavar "URL"))
       (progDesc "add new podcast URL")
     )
  <> command "update"
     (info
       (pure Update)
       (progDesc "retrieve updates from podcast URLs")
     )
  <> command "download"
     (info
       (pure Download)
       (progDesc "download episodes for all podcasts")
     )
  <> command "delete"
     (info
       (Delete <$> argument str (metavar "URL"))
       (progDesc "delete podcast and its episodes")
     )
  <> command "quit"
     (info
       (pure Quit )
       idm
     )

main = repl cmds
