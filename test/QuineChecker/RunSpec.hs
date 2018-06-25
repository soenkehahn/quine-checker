module QuineChecker.RunSpec where

import QuineChecker.Run
import System.Directory
import Development.Shake
import System.IO
import System.Exit
import System.FilePath
import Test.Hspec
import Test.Mockery.Directory
import System.IO.Silently

writeQuineFile :: String -> String -> IO ()
writeQuineFile directory code = do
  createDirectory directory
  writeFile (directory </> "quine") code
  unit $ cmd "chmod +x" (directory </> "quine")

pythonQuine :: String
pythonQuine = unlines $
  "#!/usr/bin/env python3" :
  "import sys" :
  "with open(__file__, 'r') as fh:" :
  "    sys.stdout.write(fh.read())" :
  []

pythonStderrQuine :: String
pythonStderrQuine = unlines $
  "#!/usr/bin/env python3" :
  "import sys" :
  "with open(__file__, 'r') as fh:" :
  "    sys.stderr.write(fh.read())" :
  []

pythonNonQuine :: String
pythonNonQuine = unlines $
  "#!/usr/bin/env python3" :
  "print('Hello, World!')" :
  []

spec :: Spec
spec = around_ (inTempDirectory . hSilence [stdout, stderr]) $ do
  describe "run" $ do
    describe "when pointed to directories containing a quine" $ do
      it "exits with exit code 0" $ do
        writeQuineFile "foo" pythonQuine
        run ["foo"] `shouldReturn` ExitSuccess

      it "allows quines that exit with a non-zero exitcode" $ do
        let quine = pythonQuine ++ "\nsys.exit(1)"
        writeQuineFile "foo" quine
        run ["foo"] `shouldReturn` ExitSuccess

      it "allows quines through stderr" $ do
        writeQuineFile "foo" pythonStderrQuine
        run ["foo"] `shouldReturn` ExitSuccess

      it "prints the quine" $ do
        writeQuineFile "foo" pythonQuine
        output <- hCapture_ [stderr] $ run ["foo"]
        output `shouldContain` ("this is a quine: foo/quine:\n\n" ++ pythonQuine)

      it "can checks multiple quines" $ do
        writeQuineFile "foo" pythonQuine
        writeQuineFile "bar" pythonQuine
        run ["foo", "bar"] `shouldReturn` ExitSuccess

    describe "when pointed to a directory containing an incorrect quine" $ do
      it "exits with exit code 1" $ do
        writeQuineFile "foo" pythonNonQuine
        run ["foo"] `shouldReturn` ExitFailure 1

      it "gives a nice error message" $ do
        writeQuineFile "foo" pythonNonQuine
        output <- hCapture_ [stderr] $ run ["foo"]
        output `shouldContain` "not a quine: foo/quine"

      it "outputs a nice diff" $ do
        writeQuineFile "foo" pythonNonQuine
        output <- hCapture_ [stderr] $ run ["foo"]
        let expected = unlines $
              "diff:" :
              "- #!/usr/bin/env python3" :
              "- print('Hello, World!')" :
              "+ Hello, World!" :
              []
        output `shouldContain` expected

      it "checks all given directories" $ do
        writeQuineFile "foo" pythonQuine
        writeQuineFile "bar" pythonNonQuine
        run ["foo", "bar"] `shouldReturn` ExitFailure 1

    describe "when given a directory that doesn't exist" $ do
      it "gives a nice error message" $ do
        run ["foo"] `shouldThrow` errorCall "directory not found: foo"

      it "checks all given directories" $ do
        writeQuineFile "foo" pythonQuine
        run ["foo", "bar"] `shouldThrow` errorCall "directory not found: bar"

    describe "when the directory does not contain a ./quine file" $ do
      it "gives a nice error" $ do
        createDirectory "foo"
        run ["foo"] `shouldThrow` errorCall "quine file not found: foo/quine"

    describe "when quine file is not executable" $ do
      it "gives a nice error" $ do
        createDirectory "foo"
        writeFile "foo/quine" "foo"
        run ["foo"] `shouldThrow` errorCall "executable flag not set on: foo/quine"

    describe "when the language is compiled" $ do
      it "can check compiled quines" $ do
        createDirectory "foo"
        writeFile "foo/quine.c" $ unlines $
            "#include <stdio.h>" :
            "int main() {" :
            "  FILE *file = fopen(\"foo/quine.c\", \"r\");" :
            "  int c;" :
            "  if (file) {" :
            "    while ((c = getc(file)) != EOF) {" :
            "      putchar(c);" :
            "    }" :
            "    fclose(file);" :
            "  }" :
            "  return 0;" :
            "}" :
            []
        run ["foo"] `shouldReturn` ExitSuccess
