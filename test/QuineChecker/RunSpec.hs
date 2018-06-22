module QuineChecker.RunSpec where

import QuineChecker.Run
import System.Directory
import System.Exit
import Test.Hspec
import Test.Mockery.Directory

spec :: Spec
spec = do
  describe "run" $ do
    describe "when pointed to a directory containing a quine" $ do
      it "exits with exit code 0" $ do
        inTempDirectory $ do
          createDirectory "foo"
          writeFile "foo/quine" $ unlines $
            "#!/usr/bin/env python3" :
            "" :
            "with open(__file__, 'r') as fh:" :
            "    print(fh.read())" :
            []
          run "foo" `shouldReturn` ExitSuccess

    describe "when pointed to a directory containing an incorrect quine" $ do
      it "exits with exit code 1" $ do
        pending
