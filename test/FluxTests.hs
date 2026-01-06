module FluxTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import System.Process
import System.Exit
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import System.Random (randomRIO)

runGlados :: FilePath -> IO (ExitCode, String, String)
runGlados file = readProcessWithExitCode "stack" ["exec", "glados", "--", "-i", file] ""

runFlux :: String -> IO (ExitCode, String, String)
runFlux content = do
  tmpDir <- getTemporaryDirectory
  randNum <- randomRIO (1, 1000000 :: Int)
  let file = tmpDir </> ("flux-test-" ++ show randNum ++ ".flux")
  writeFile file content
  result <- runGlados file
  removeFile file
  return result

tests :: IO TestTree
tests = return $ testGroup "Flux Interpreter Tests"
  [ testGroup "Arithmetic"
      [ testCase "simple addition" $ do
          (_, out, _) <- runFlux "print(1 + 2)"
          out @?= "3\n"
      , testCase "multiplication" $ do
          (_, out, _) <- runFlux "print(3 * 4)"
          out @?= "12\n"
      , testCase "subtraction" $ do
          (_, out, _) <- runFlux "print(10 - 3)"
          out @?= "7\n"
      , testCase "division" $ do
          (_, out, _) <- runFlux "print(20 / 4)"
          out @?= "5\n"
      , testCase "modulo" $ do
          (_, out, _) <- runFlux "print(17 % 5)"
          out @?= "2\n"
      ]
  , testGroup "Booleans"
      [ testCase "true" $ do
          (_, out, _) <- runFlux "print(true)"
          out @?= "#t\n"
      , testCase "false" $ do
          (_, out, _) <- runFlux "print(false)"
          out @?= "#f\n"
      , testCase "comparison <" $ do
          (_, out, _) <- runFlux "print(1 < 2)"
          out @?= "#t\n"
      ]
  , testGroup "Variables"
      [ testCase "let binding" $ do
          (_, out, _) <- runFlux "let x = 42\nprint(x)\n0"
          out @?= "42\n"
      ]
  , testGroup "Functions"
      [ testCase "simple function" $ do
          (_, out, _) <- runFlux "fn double(x) = x * 2\nprint(double(5))\n0"
          out @?= "10\n"
      , testCase "recursive function" $ do
          (_, out, _) <- runFlux "fn fact(n) = if n <= 1 { 1 } else { n * fact(n - 1) }\nprint(fact(5))\n0"
          out @?= "120\n"
      ]
  , testGroup "If-Else"
      [ testCase "if true branch" $ do
          (_, out, _) <- runFlux "print(if true { 1 } else { 2 })\n0"
          out @?= "1\n"
      , testCase "if false branch" $ do
          (_, out, _) <- runFlux "print(if false { 1 } else { 2 })\n0"
          out @?= "2\n"
      ]
  , testGroup "Exit Codes"
      [ testCase "return 0" $ do
          (code, _, _) <- runFlux "0"
          code @?= ExitSuccess
      , testCase "return non-zero" $ do
          (code, _, _) <- runFlux "42"
          code @?= ExitFailure 42
      ]
  , testGroup "Errors"
      [ testCase "parse error exits 84" $ do
          (code, _, _) <- runFlux "fn ("
          code @?= ExitFailure 84
      , testCase "unbound variable exits 84" $ do
          (code, _, _) <- runFlux "print(undefined_var)"
          code @?= ExitFailure 84
      , testCase "division by zero exits 84" $ do
          (code, _, _) <- runFlux "print(1 / 0)"
          code @?= ExitFailure 84
      ]
  ]
