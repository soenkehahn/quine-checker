
module QuineChecker.Run where

import System.Exit
import Development.Shake hiding (doesDirectoryExist)
import Control.Monad
import System.Directory
import Control.Exception
import System.FilePath

run :: [String] -> IO ExitCode
run (directory : rest) = do
  exitCode <- checkQuine directory
  case exitCode of
    ExitSuccess -> run rest
    ExitFailure _ -> return exitCode
run [] = return ExitSuccess

checkQuine directory = do
  directoryExists <- doesDirectoryExist directory
  when (not directoryExists) $
    throwIO $ ErrorCall ("directory does not exist: " ++ directory)
  Stdout stdout <- cmd "./quine" (Cwd directory)
  code <- readFile (directory </> "quine")
  return $ if code == stdout
    then ExitSuccess
    else ExitFailure 1
