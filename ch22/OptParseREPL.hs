module OptParseREPL (repl) where

import Control.Monad                   (when)
import Data.List                       (isInfixOf)
import Data.List.Split                 (splitOn)
import Data.Semigroup                  (Semigroup ((<>)))
import Options.Applicative             (CommandFields, Mod,
                                        ParserFailure (execFailure),
                                        ParserHelp (helpBody, helpError,
                                                    helpSuggestions, helpUsage),
                                        ParserPrefs (prefShowHelpOnEmpty),
                                        ParserResult (Failure, Success),
                                        defaultPrefs, execParserPure,
                                        hsubparser, idm, info, metavar)
import Options.Applicative.Help.Chunk  (Chunk (unChunk), extractChunk)
import Options.Applicative.Help.Pretty (Doc, displayS, renderPretty)

renderHelpChunk :: Int -> (ParserHelp -> Chunk Doc) -> ParserHelp -> String
renderHelpChunk cols f
  = (`displayS` "") . renderPretty 1.0 cols . extractChunk . f

repl :: Mod CommandFields a -> IO a
repl cmds =
  -- This is to filter out "usage" section when nothing is entered
  let parser = hsubparser $ cmds <> metavar "hide usage"
  in repl0 parser

repl0 parser = do
  printMenu parser
  putStr "> "
  args <- filter (not . null) . splitOn " " <$> getLine
  case execParserPure defaultPrefs (info parser idm) args of
    Success c -> return c
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

      repl0 parser
    _ -> error "Completion not supported"

printMenu parser = do
  case execParserPure
       defaultPrefs { prefShowHelpOnEmpty = True }
       (info parser idm) [] of
    Failure f -> do
      let (help, _, cols) = execFailure f $ ""
      putStrLn . renderHelpChunk cols helpBody $ help
    _ -> error "Print menu should fail command parsing"

