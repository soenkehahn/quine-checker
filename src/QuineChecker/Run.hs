
module QuineChecker.Run where

import System.Exit
import Development.Shake hiding (doesDirectoryExist, doesFileExist)
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

checkQuine :: String -> IO ExitCode
checkQuine directory = do
  directoryExists <- doesDirectoryExist directory
  when (not directoryExists) $
    throwIO $ ErrorCall ("directory not found: " ++ directory)
  quineExists <- doesFileExist (directory </> "quine")
  when (not quineExists) $
    throwIO $ ErrorCall ("quine file not found: " ++ directory </> "quine")
  Stdout stdout <- cmd (directory </> "quine")
  code <- readFile (directory </> "quine")
  return $ if code == stdout
    then ExitSuccess
    else ExitFailure 1
