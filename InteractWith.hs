module InteractWith (mainWith, interactWith) where
import System.Environment (getArgs)

interactWith interact inputFile outputFile = do
  file <- readFile inputFile
  writeFile outputFile $ interact file

mainWith interact = do
  args <- getArgs
  case args of
    [input, output] -> interactWith interact input output
    _ -> error "Exactly two arguments are expected: input output"
