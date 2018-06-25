
module QuineChecker.Run where

import Prelude hiding (log)
import System.Exit
import Data.List
import System.IO
import System.Posix.Files
import Development.Shake hiding (doesDirectoryExist, doesFileExist, getDirectoryContents)
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

checkQuine :: FilePath -> IO ExitCode
checkQuine directory = do
  directoryExists <- doesDirectoryExist directory
  when (not directoryExists) $
    throwIO $ ErrorCall ("directory not found: " ++ directory)
  sourceFile <- compile directory
  let executableFile = directory </> "quine"
  executableExists <- doesFileExist executableFile
  when (not executableExists) $
    throwIO $ ErrorCall ("quine file not found: " ++ executableFile)
  isExecutable <- fileAccess executableFile False False True
  when (not isExecutable) $
    throwIO $ ErrorCall ("executable flag not set on: " ++ executableFile)
  (Exit _, Stdout stdout, Stderr stderr) <- cmd executableFile
  let output = stdout ++ stderr
  code <- readFile sourceFile
  if code == output
    then do
      log ("this is a quine: " ++ sourceFile ++ ":\n")
      log code
      return ExitSuccess
    else do
      log ("not a quine: " ++ sourceFile)
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

-- * compiled languages

compile :: FilePath -> IO FilePath
compile directory = do
  files <- filter ("quine." `isPrefixOf`) <$>
    getDirectoryContents directory
  case files of
    [quineSourceFile] -> do
      unit $ cmd "gcc" quineSourceFile "-o quine" (Cwd directory)
      return (directory </> quineSourceFile)
    [] -> do
      return (directory </> "quine")
    _ : _ : _ -> do
      throwIO $ ErrorCall ("multiple quine files found: " ++ show files)
