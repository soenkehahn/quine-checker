#!/usr/bin/env stack
{- stack script --resolver=lts-11.14 -}

{-# LANGUAGE ViewPatterns #-}

import Control.Exception
import Control.Monad
import Data.Char
import Data.Foldable
import Data.List
import Development.Shake (unit, cmd, Stdouterr(..), CmdOption(..))
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
    Stdouterr (strip -> output) <- cmd (EchoStderr True) (EchoStdout True)
      "docker run --rm -t -v"
      (currentDir </> "base-image/tests" </> file ++ ":" ++ "/root" </> file)
      "soenkehahn/rc-quines-candidate" ("/root" </> file)
    when (output /= "Hello, World!") $
      throwIO $ ErrorCall ("unexpected output: " ++ show output)
  unit $ cmd "docker build --tag soenkehahn/rc-quines ./base-image"

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
