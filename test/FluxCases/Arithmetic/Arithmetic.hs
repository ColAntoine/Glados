module FluxCases.Arithmetic.Arithmetic (tests) where

import Test.Tasty
import FluxCases.TestUtils

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
