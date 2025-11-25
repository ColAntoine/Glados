module Main where

import Test.Tasty
import qualified LispCases.Call.Call as CallTest
import qualified LispCases.Builtins.Builtins as BuiltinsTest
import qualified LispCases.Error.Error as ErrorTest
import qualified LispCases.Factorial.Factorial as FactorialTest
import qualified LispCases.Foo.Foo as FooTest
import qualified LispCases.Function.Function as FunctionTest
import qualified LispCases.If.If as IfTest
import qualified LispCases.Lambda.Lambda as LambdaTest
import qualified LispCases.Superior.Superior as SuperiorTest

main :: IO ()
main = do
  callTests <- CallTest.tests
  builtinsTests <- BuiltinsTest.tests
  errorTests <- ErrorTest.tests
  factorialTests <- FactorialTest.tests
  fooTests <- FooTest.tests
  functionTests <- FunctionTest.tests
  ifTests <- IfTest.tests
  lambdaTests <- LambdaTest.tests
  superiorTests <- SuperiorTest.tests
  defaultMain $ testGroup "Glados Tests"
    [ callTests
    , builtinsTests
    , errorTests
    , factorialTests
    , fooTests
    , functionTests
    , ifTests
    , lambdaTests
    , superiorTests
    ]