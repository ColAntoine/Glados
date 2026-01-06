module FluxCases.Arithmetic.Arithmetic (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import System.Process
import System.Exit
import System.Directory (getTemporaryDirectory, removeFile, doesFileExist)
import System.FilePath ((</>))
import System.Random (randomRIO)
import Control.Monad (when)
import Control.Exception (catch, SomeException)

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

-- | Run a test in both interpreter and compiler mode
runBothModes :: FilePath -> IO (String, String)
runBothModes testFile = do
  (interpCode, interpOut, _) <- runInterpreter testFile
  tmpDir <- getTemporaryDirectory
  randNum <- randomRIO (1, 1000000 :: Int)
  let outBin = tmpDir </> ("flux-test-" ++ show randNum)
  (compCode, compOut, _) <- runCompiler testFile outBin
  case (interpCode, compCode) of
    (ExitSuccess, ExitSuccess) -> return (interpOut, compOut)
    (ExitFailure _, _) -> return (interpOut, "")
    _ -> return (interpOut, compOut)

-- | Test a file and verify both modes produce same output
testFluxFile :: String -> FilePath -> String -> TestTree
testFluxFile name filePath expectedOutput =
  testCase name $ do
    (interpOut, compOut) <- runBothModes filePath
    interpOut @?= expectedOutput
    compOut @?= expectedOutput

tests :: IO TestTree
tests = return $ testGroup "Arithmetic Tests"
  [ testFluxFile "addition" "test/FluxCases/Arithmetic/addition.flux" "3\n"
  , testFluxFile "subtraction" "test/FluxCases/Arithmetic/subtraction.flux" "7\n"
  , testFluxFile "multiplication" "test/FluxCases/Arithmetic/multiplication.flux" "12\n"
  , testFluxFile "division" "test/FluxCases/Arithmetic/division.flux" "5\n"
  , testFluxFile "modulo" "test/FluxCases/Arithmetic/modulo.flux" "2\n"
  , testFluxFile "precedence" "test/FluxCases/Arithmetic/precedence.flux" "7\n"
  , testFluxFile "parentheses" "test/FluxCases/Arithmetic/parentheses.flux" "9\n"
  ]
