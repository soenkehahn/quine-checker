
module QuineChecker.Run where

import System.Exit
import Development.Shake
import System.FilePath

run :: String -> IO ExitCode
run directory = do
  Stdout stdout <- cmd "./quine" (Cwd directory)
  code <- readFile (directory </> "quine")
  return $ if code == stdout
    then ExitSuccess
    else ExitFailure 1
