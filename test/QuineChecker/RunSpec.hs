module QuineChecker.RunSpec where

import QuineChecker.Run
import System.Directory
import Development.Shake
import System.Exit
import Test.Hspec
import Test.Mockery.Directory
import System.IO.Silently

writeQuineFile :: [String] -> IO ()
writeQuineFile lines = do
  createDirectory "foo"
  writeFile "foo/quine" $ unlines lines
  unit $ cmd "chmod +x foo/quine"

spec :: Spec
spec = around_ silence $ do
  describe "run" $ do
    describe "when pointed to a directory containing a quine" $ do
      it "exits with exit code 0" $ do
        inTempDirectory $ do
          writeQuineFile $
            "#!/usr/bin/env python3" :
            "import sys" :
            "with open(__file__, 'r') as fh:" :
            "    sys.stdout.write(fh.read())" :
            []
          run "foo" `shouldReturn` ExitSuccess

      it "allows quines through stderr" $ do
        pending

      it "prints the quine" $ do
        pending

    describe "when pointed to a directory containing an incorrect quine" $ do
      it "exits with exit code 1" $ do
        inTempDirectory $ do
          writeQuineFile $
            "#!/usr/bin/env python3" :
            "" :
            "print('Hello, World!')" :
            []
          run "foo" `shouldReturn` ExitFailure 1

      it "outputs a nice diff" $ do
        pending

    describe "when ./quine doesn't exist" $ do
      it "throws a good error message" $ do
        pending
