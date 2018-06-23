
import QuineChecker.Run
import System.Environment
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  exitCode <- run args
  exitWith exitCode
