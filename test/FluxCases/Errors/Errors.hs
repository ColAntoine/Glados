module FluxCases.Errors.Errors (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import System.Process
import System.Exit
import System.Directory (getTemporaryDirectory, removeFile, doesFileExist)
import System.FilePath ((</>))
import System.Random (randomRIO)
import Control.Monad (when)

-- | Run glados in interpreter mode
runInterpreter :: FilePath -> IO (ExitCode, String, String)
runInterpreter file = readProcessWithExitCode "stack" ["exec", "glados", "--", "-i", file] ""

-- | Run glados in compiler mode - compiles and executes the binary
runCompiler :: FilePath -> FilePath -> IO (ExitCode, String, String)
runCompiler file outBin = do
  (compileCode, _, compileErr) <- readProcessWithExitCode "stack" 
    ["exec", "glados", "--", "-c", file, "-o", outBin] ""
  case compileCode of
    ExitFailure _ -> return (compileCode, "", compileErr)
    ExitSuccess -> do
      result <- readProcessWithExitCode outBin [] ""
      binExists <- doesFileExist outBin
      when binExists $ removeFile outBin
      return result

-- | Test that a file causes a parse error (exit code 84)
testParseError :: String -> FilePath -> TestTree
testParseError name filePath =
  testCase (name ++ " - interpreter") $ do
    (code, _, _) <- runInterpreter filePath
    code @?= ExitFailure 84

tests :: IO TestTree
tests = return $ testGroup "Error Tests"
  [ testParseError "parse error" "test/FluxCases/Errors/parse_error.flux"
  ]
