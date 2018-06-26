#!/usr/bin/env stack
{- stack script --resolver=lts-11.14 -}

{-# LANGUAGE ViewPatterns #-}

import Control.Exception
import Control.Monad
import Data.Char
import Data.Foldable
import Data.List
import Development.Shake (unit, cmd, Stdouterr(..), CmdOption(..), Exit(..))
import System.Directory
import System.FilePath

main :: IO ()
main = do
  unit $ cmd "stack test"
  unit $ cmd "stack install --local-bin-path base-image/dist"
  unit $ cmd "docker build --tag soenkehahn/rc-quines-candidate ./base-image"
  files <- filter (not . ("." `isPrefixOf`)) <$>
    getDirectoryContents "base-image/tests"
  forM_ files $ \ file -> do
    putStrLn ("testing " ++ file)
    currentDir <- getCurrentDirectory
    (Exit _, Stdouterr (strip -> output)) <- cmd (EchoStderr True) (EchoStdout True)
      "docker run --rm -t -v"
      (currentDir </> "base-image/tests" </> file ++ ":" ++ mountPoint file)
      "soenkehahn/rc-quines-candidate" "quine-checker" "/root/foo"
    when (not ( "not a quine:" `isPrefixOf` output)) $
      throwIO $ ErrorCall ("unexpected output: " ++ show output)
  unit $ cmd "docker build --tag soenkehahn/rc-quines ./base-image"

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

mountPoint :: FilePath -> FilePath
mountPoint path = case takeExtension path of
  ".c" -> "/root/foo/quine.c"
  ".rs" -> "/root/foo/quine.rs"
  ".go" -> "/root/foo/quine.go"
  _ -> "/root/foo/quine"
