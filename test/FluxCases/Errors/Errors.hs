module FluxCases.Errors.Errors (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import System.Exit
import FluxCases.TestUtils

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
