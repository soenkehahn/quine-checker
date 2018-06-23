
module QuineChecker.Run where

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
  Stdout stdout <- cmd quineFile
  code <- readFile quineFile
  if code == stdout
    then do
      hPutStrLn stderr ("this is a quine: " ++ quineFile ++ ":\n")
      hPutStrLn stderr code
      return ExitSuccess
    else do
      hPutStrLn stderr ("not a quine: " ++ quineFile)
      hPutStrLn stderr ("diff:\n" ++ diff code stdout)
      return $ ExitFailure 1

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
