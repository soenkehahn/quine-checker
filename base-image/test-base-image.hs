#!/usr/bin/env stack
{- stack script --resolver=lts-11.14 -}

{-# LANGUAGE ViewPatterns #-}

import Control.Exception
import Control.Monad
import Data.Char
import Data.Foldable
import Data.List
import Development.Shake (unit, cmd, Stdout(..), CmdOption(..))
import System.Directory
import System.FilePath

main :: IO ()
main = do
  files <- filter (not . ("." `isPrefixOf`)) <$>
    getDirectoryContents "base-image/tests"
  forM_ files $ \ file -> do
    currentDir <- getCurrentDirectory
    Stdout (strip -> output) <- cmd (EchoStderr True) (EchoStdout True)
      "docker run --rm -t -v"
      (currentDir </> "base-image/tests" </> file ++ ":" ++ "/root" </> file)
      "soenkehahn/rc-quines-candidate" ("/root" </> file)
    when (output /= "Hello, World!") $
      throwIO $ ErrorCall ("unexpected output: " ++ show output)

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
