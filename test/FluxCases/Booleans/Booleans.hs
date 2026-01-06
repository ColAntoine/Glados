module FluxCases.Booleans.Booleans (tests) where

import Test.Tasty
import FluxCases.TestUtils

tests :: IO TestTree
tests = return $ testGroup "Boolean Tests"
  [ testFluxFile "true literal" "test/FluxCases/Booleans/true_literal.flux" "#t\n"
  , testFluxFile "false literal" "test/FluxCases/Booleans/false_literal.flux" "#f\n"
  , testFluxFile "less than" "test/FluxCases/Booleans/less_than.flux" "#t\n"
  , testFluxFile "greater than" "test/FluxCases/Booleans/greater_than.flux" "#t\n"
  , testFluxFile "equality" "test/FluxCases/Booleans/equality.flux" "#t\n"
  , testFluxFile "inequality" "test/FluxCases/Booleans/inequality.flux" "#t\n"
  ]
