
module QuineChecker.Run where

import Prelude hiding (log)
import System.Exit
import Data.List
import System.IO
import System.Posix.Files
import Development.Shake hiding (doesDirectoryExist, doesFileExist)
import Control.Monad
import System.Directory
import Control.Exception
import System.FilePath
import Data.Algorithm.Diff

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
  (Exit _, Stdout stdout, Stderr stderr) <- cmd quineFile
  let output = stdout ++ stderr
  code <- readFile quineFile
  if code == output
    then do
      log ("this is a quine: " ++ quineFile ++ ":\n")
      log code
      return ExitSuccess
    else do
      log ("not a quine: " ++ quineFile)
      log ("diff:\n" ++ diff code output)
      return $ ExitFailure 1

log :: String -> IO ()
log = hPutStrLn stderr

diff :: String -> String -> String
diff a b =
  intercalate "\n" $ map renderDiff $
    getDiff
      (map (: []) (lines a))
      (map (: []) (lines b))
  where
    renderDiff :: Diff [String] -> String
    renderDiff d = concat $ case d of
      Both f _ -> map ("  " ++) f
      First f -> map ("- " ++) f
      Second s -> map ("+ " ++) s
