module FluxCases.Complex.Complex (tests) where

import Test.Tasty
import FluxCases.TestUtils

tests :: IO TestTree
tests = return $ testGroup "Complex Tests"
  [ testFluxFile "expression in function" "test/FluxCases/Complex/expr_in_function.flux" "10\n"
  , testFluxFile "mixed operations" "test/FluxCases/Complex/mixed_ops.flux" "30\n"
  ]
