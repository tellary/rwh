import Control.Lens                    ()
import Control.Monad                   (when)
import Data.Char                       (isSpace)
import Data.List                       (isInfixOf)
import Data.List.Split                 (splitOn)
import Data.Semigroup                  (Semigroup ((<>)))
import Options.Applicative             (ParserFailure (execFailure),
                                        ParserHelp (helpBody, helpError,
                                                    helpSuggestions,
                                                    helpUsage),
                                        ParserPrefs (prefShowHelpOnEmpty),
                                        ParserResult (Failure, Success),
                                        argument, command, defaultPrefs,
                                        execParserPure, hsubparser, idm, info,
                                        metavar, progDesc, str)
import Options.Applicative.Help.Chunk  (Chunk (unChunk), extractChunk)
import Options.Applicative.Help.Pretty (Doc, displayS, renderPretty)

data Command = Add String
             | Update
             | Download
             | Delete String
             | Quit
             deriving Show

opts = hsubparser
       (  command "add"
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
       -- This is to filter out "usage" section when nothing is entered
       <> metavar "hide usage"
       )

renderHelpChunk :: Int -> (ParserHelp -> Chunk Doc) -> ParserHelp -> String
renderHelpChunk cols f
  = (`displayS` "") . renderPretty 1.0 cols . extractChunk . f

main = do
  printMenu
  args <- filter (not . null) . splitOn " " <$> getLine
  case execParserPure defaultPrefs (info opts idm) args of
    Success Quit -> return ()
    Success c -> putStrLn . show $ c
    Failure f -> do
      let (help, _, cols) = execFailure f $ ""

      -- Show error if not "Invalid argument"
      let error = renderHelpChunk cols helpError $ help
      when (not $ "Invalid argument" `isInfixOf` error) $ putStrLn error

      -- Show suggestions if any
      case unChunk . helpSuggestions $ help of
        Just s  -> do
          putStrLn . (`displayS` "") . renderPretty 1.0 cols $ s
        Nothing -> return ()

      -- Show usage unless "hide usage" metavar is present.
      -- This happens when no command is typed in properly.
      let usage = renderHelpChunk cols helpUsage $ help
      -- Replacing "Usage:  " with "Usage: "
      let usage' = "Usage: " ++ (drop 8 usage)
      when (not $ "hide usage" `isInfixOf` usage) $ putStrLn usage'

      main
    _ -> error "Completion not supported"

printMenu = do
  case execParserPure
       defaultPrefs { prefShowHelpOnEmpty = True }
       (info opts idm) [] of
    Failure f -> do
      let (help, _, cols) = execFailure f $ ""
      putStrLn . renderHelpChunk cols helpBody $ help
    _ -> error "Print menu should fail command parsing"
