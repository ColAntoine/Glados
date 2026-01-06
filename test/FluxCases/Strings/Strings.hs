module FluxCases.Strings.Strings (tests) where

import Test.Tasty
import FluxCases.TestUtils

tests :: IO TestTree
tests = return $ testGroup "String Tests"
  [ testFluxFile "string print" "test/FluxCases/Strings/string_print.flux" "hello"
  , testFluxFile "string with spaces" "test/FluxCases/Strings/string_with_spaces.flux" "hello world"
  ]
