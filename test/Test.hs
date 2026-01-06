module Main where

import Test.Tasty
import qualified FluxCases.Arithmetic.Arithmetic as ArithmeticTests
import qualified FluxCases.Booleans.Booleans as BooleanTests
import qualified FluxCases.Variables.Variables as VariableTests
import qualified FluxCases.Functions.Functions as FunctionTests
import qualified FluxCases.IfElse.IfElse as IfElseTests
import qualified FluxCases.Strings.Strings as StringTests
import qualified FluxCases.Complex.Complex as ComplexTests
import qualified FluxCases.Errors.Errors as ErrorTests

main :: IO ()
main = do
  arithmeticTests <- ArithmeticTests.tests
  booleanTests <- BooleanTests.tests
  variableTests <- VariableTests.tests
  functionTests <- FunctionTests.tests
  ifElseTests <- IfElseTests.tests
  stringTests <- StringTests.tests
  complexTests <- ComplexTests.tests
  errorTests <- ErrorTests.tests
  defaultMain $ testGroup "Glados Tests"
    [ arithmeticTests
    , booleanTests
    , variableTests
    , functionTests
    , ifElseTests
    , stringTests
    , complexTests
    , errorTests
    ]
