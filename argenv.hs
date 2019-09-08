import System.Environment
import Control.Exception

lineEnv = do
  putStrLn "What env variable?"
  envName <- getLine
  printEnv envName

argEnv = do
  args <- getArgs
  case args of
    [envName] -> printEnv envName
    _ -> putStrLn "Exactly one argument should be provided"

printEnv envName = do
  envValueEither <- try (getEnv envName)
  case envValueEither of
    (Right envValue) -> do
      putStr "Env variable "
      putStr envName
      putStr ": "
      putStr envValue
      putStrLn ""
    Left (SomeException _) ->
      putStrLn $ "No env variable " ++ envName

main = argEnv
  
