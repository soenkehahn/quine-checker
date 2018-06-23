
module QuineChecker.Run where

import System.Exit
import System.IO
import System.Posix.Files
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
  let quineFile = directory </> "quine"
  quineExists <- doesFileExist quineFile
  when (not quineExists) $
    throwIO $ ErrorCall ("quine file not found: " ++ quineFile)
  executable <- fileAccess quineFile False False True
  when (not executable) $
    throwIO $ ErrorCall ("executable flag not set on: " ++ quineFile)
  Stdout stdout <- cmd quineFile
  code <- readFile quineFile
  if code == stdout
    then return ExitSuccess
    else do
      hPutStrLn stderr ("not a quine: " ++ quineFile)
      return $ ExitFailure 1
