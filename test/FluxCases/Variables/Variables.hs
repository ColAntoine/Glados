module FluxCases.Variables.Variables (tests) where

import Test.Tasty
import FluxCases.TestUtils

tests :: IO TestTree
tests = return $ testGroup "Variable Tests"
  [ testFluxFile "let binding" "test/FluxCases/Variables/let_binding.flux" "42\n"
  , testFluxFile "multiple lets" "test/FluxCases/Variables/multiple_lets.flux" "30\n"
  ]
