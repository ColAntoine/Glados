module FluxCases.Functions.Functions (tests) where

import Test.Tasty
import FluxCases.TestUtils

tests :: IO TestTree
tests = return $ testGroup "Function Tests"
  [ testFluxFile "simple function" "test/FluxCases/Functions/simple_function.flux" "10\n"
  , testFluxFile "two params" "test/FluxCases/Functions/two_params.flux" "7\n"
  , testFluxFile "factorial" "test/FluxCases/Functions/factorial.flux" "120\n"
  , testFluxFile "fibonacci" "test/FluxCases/Functions/fibonacci.flux" "55\n"
  , testFluxFile "nested calls" "test/FluxCases/Functions/nested_calls.flux" "12\n"
  ]
