module FluxTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import System.Process
import System.Exit
import System.Directory (getTemporaryDirectory, removeFile, doesFileExist)
import System.FilePath ((</>))
import System.Random (randomRIO)
import Control.Monad (when)

-- | Mode for running tests
data Mode = Interpret | Compile deriving (Show, Eq)

-- | Run glados in interpreter mode
runInterpreter :: FilePath -> IO (ExitCode, String, String)
runInterpreter file = readProcessWithExitCode "stack" ["exec", "glados", "--", "-i", file] ""

-- | Run glados in compiler mode - compiles and executes the binary
runCompiler :: FilePath -> FilePath -> IO (ExitCode, String, String)
runCompiler file outBin = do
  -- Compile
  (compileCode, _, compileErr) <- readProcessWithExitCode "stack" 
    ["exec", "glados", "--", "-c", file, "-o", outBin] ""
  case compileCode of
    ExitFailure _ -> return (compileCode, "", compileErr)
    ExitSuccess -> do
      -- Run the compiled binary
      result <- readProcessWithExitCode outBin [] ""
      -- Clean up the binary
      binExists <- doesFileExist outBin
      when binExists $ removeFile outBin
      return result

-- | Run flux code in a specific mode
runFluxMode :: Mode -> String -> IO (ExitCode, String, String)
runFluxMode mode content = do
  tmpDir <- getTemporaryDirectory
  randNum <- randomRIO (1, 1000000 :: Int)
  let file = tmpDir </> ("flux-test-" ++ show randNum ++ ".flux")
  let outBin = tmpDir </> ("flux-test-bin-" ++ show randNum)
  writeFile file content
  result <- case mode of
    Interpret -> runInterpreter file
    Compile -> runCompiler file outBin
  removeFile file
  return result

-- | Test case that expects specific output
data TestSpec = TestSpec
  { tsName :: String
  , tsCode :: String
  , tsExpectedOut :: Maybe String      -- Expected stdout (Nothing = don't check)
  , tsExpectedCode :: Maybe ExitCode   -- Expected exit code (Nothing = don't check)
  , tsCompilerSupported :: Bool        -- Whether compiler should handle this test
  }

-- | Create a test that checks both interpreter and compiler
mkDualTest :: TestSpec -> TestTree
mkDualTest spec = testGroup (tsName spec) $
  [ testCase "interpreter" $ do
      (code, out, _) <- runFluxMode Interpret (tsCode spec)
      case tsExpectedOut spec of
        Just expected -> out @?= expected
        Nothing -> return ()
      case tsExpectedCode spec of
        Just expected -> code @?= expected
        Nothing -> return ()
  ] ++
  [ testCase "compiler" $ do
      (code, out, _) <- runFluxMode Compile (tsCode spec)
      case tsExpectedOut spec of
        Just expected -> out @?= expected
        Nothing -> return ()
      case tsExpectedCode spec of
        Just expected -> code @?= expected
        Nothing -> return ()
  | tsCompilerSupported spec
  ]

-- | Create a test for errors (interpreter only or both)
mkErrorTest :: String -> String -> Bool -> TestTree
mkErrorTest name code compilerSupported = testGroup name $
  [ testCase "interpreter" $ do
      (exitCode, _, _) <- runFluxMode Interpret code
      exitCode @?= ExitFailure 84
  ] ++
  [ testCase "compiler" $ do
      (exitCode, _, _) <- runFluxMode Compile code
      exitCode @?= ExitFailure 84
  | compilerSupported
  ]

-- | All test specifications
testSpecs :: [TestSpec]
testSpecs =
  -- Arithmetic tests
  [ TestSpec "addition" "print(1 + 2)" (Just "3\n") Nothing True
  , TestSpec "multiplication" "print(3 * 4)" (Just "12\n") Nothing True
  , TestSpec "subtraction" "print(10 - 3)" (Just "7\n") Nothing True
  , TestSpec "division" "print(20 / 4)" (Just "5\n") Nothing True
  , TestSpec "modulo" "print(17 % 5)" (Just "2\n") Nothing True
  , TestSpec "precedence" "print(1 + 2 * 3)" (Just "7\n") Nothing True
  , TestSpec "parentheses" "print((1 + 2) * 3)" (Just "9\n") Nothing True
  
  -- Boolean tests
  , TestSpec "true literal" "print(true)" (Just "#t\n") Nothing True
  , TestSpec "false literal" "print(false)" (Just "#f\n") Nothing True
  , TestSpec "less than" "print(1 < 2)" (Just "#t\n") Nothing True
  , TestSpec "greater than" "print(5 > 3)" (Just "#t\n") Nothing True
  , TestSpec "less or equal" "print(2 <= 2)" (Just "#t\n") Nothing True
  , TestSpec "greater or equal" "print(3 >= 4)" (Just "#f\n") Nothing True
  , TestSpec "equality" "print(5 == 5)" (Just "#t\n") Nothing True
  , TestSpec "inequality" "print(5 != 3)" (Just "#t\n") Nothing True
  
  -- Variable tests
  , TestSpec "let binding" "let x = 42\nprint(x)\n0" (Just "42\n") (Just ExitSuccess) True
  , TestSpec "multiple lets" "let x = 10\nlet y = 20\nprint(x + y)\n0" (Just "30\n") (Just ExitSuccess) True
  
  -- Function tests
  , TestSpec "simple function" "fn double(x) = x * 2\nprint(double(5))\n0" (Just "10\n") (Just ExitSuccess) True
  , TestSpec "two params" "fn add(a, b) = a + b\nprint(add(3, 4))\n0" (Just "7\n") (Just ExitSuccess) True
  , TestSpec "recursive factorial" "fn fact(n) = if n <= 1 { 1 } else { n * fact(n - 1) }\nprint(fact(5))\n0" (Just "120\n") (Just ExitSuccess) True
  , TestSpec "recursive fibonacci" "fn fib(n) = if n <= 1 { n } else { fib(n-1) + fib(n-2) }\nprint(fib(10))\n0" (Just "55\n") (Just ExitSuccess) True
  
  -- If-else tests
  , TestSpec "if true branch" "print(if true { 1 } else { 2 })\n0" (Just "1\n") (Just ExitSuccess) True
  , TestSpec "if false branch" "print(if false { 1 } else { 2 })\n0" (Just "2\n") (Just ExitSuccess) True
  , TestSpec "if with comparison" "print(if 5 > 3 { 100 } else { 200 })\n0" (Just "100\n") (Just ExitSuccess) True
  
  -- Exit code tests
  , TestSpec "exit 0" "0" Nothing (Just ExitSuccess) True
  , TestSpec "exit 42" "42" Nothing (Just (ExitFailure 42)) True
  , TestSpec "exit from expr" "1 + 2" Nothing (Just (ExitFailure 3)) True
  
  -- String tests
  , TestSpec "string print" "print(\"hello\")\n0" (Just "hello\n") (Just ExitSuccess) True
  
  -- Complex tests
  , TestSpec "nested calls" "fn inc(x) = x + 1\nfn double(x) = x * 2\nprint(double(inc(5)))\n0" (Just "12\n") (Just ExitSuccess) True
  , TestSpec "expression in function" "fn calc(x) = (x + 1) * 2\nprint(calc(4))\n0" (Just "10\n") (Just ExitSuccess) True
  ]

-- | Error test specifications: (name, code, compiler_handles_it)
errorSpecs :: [(String, String, Bool)]
errorSpecs =
  [ ("parse error", "fn (", True)  -- Both should fail on parse error
  , ("unbound variable", "print(undefined_var)", False)  -- Compiler doesn't check this at compile time
  , ("division by zero", "print(1 / 0)", False)  -- Runtime error, compiler doesn't detect
  ]

tests :: IO TestTree
tests = return $ testGroup "Flux Tests (Interpreter & Compiler)"
  [ testGroup "Arithmetic" $ map mkDualTest $ filter (isArithmetic . tsName) testSpecs
  , testGroup "Booleans" $ map mkDualTest $ filter (isBoolean . tsName) testSpecs
  , testGroup "Variables" $ map mkDualTest $ filter (isVariable . tsName) testSpecs
  , testGroup "Functions" $ map mkDualTest $ filter (isFunction . tsName) testSpecs
  , testGroup "If-Else" $ map mkDualTest $ filter (isIfElse . tsName) testSpecs
  , testGroup "Exit Codes" $ map mkDualTest $ filter (isExit . tsName) testSpecs
  , testGroup "Strings" $ map mkDualTest $ filter (isString . tsName) testSpecs
  , testGroup "Complex" $ map mkDualTest $ filter (isComplex . tsName) testSpecs
  , testGroup "Errors" $ map (\(n, c, s) -> mkErrorTest n c s) errorSpecs
  ]
  where
    isArithmetic n = n `elem` ["addition", "multiplication", "subtraction", "division", "modulo", "precedence", "parentheses"]
    isBoolean n = n `elem` ["true literal", "false literal", "less than", "greater than", "less or equal", "greater or equal", "equality", "inequality"]
    isVariable n = n `elem` ["let binding", "multiple lets"]
    isFunction n = n `elem` ["simple function", "two params", "recursive factorial", "recursive fibonacci"]
    isIfElse n = n `elem` ["if true branch", "if false branch", "if with comparison"]
    isExit n = n `elem` ["exit 0", "exit 42", "exit from expr"]
    isString n = n `elem` ["string print"]
    isComplex n = n `elem` ["nested calls", "expression in function"]
