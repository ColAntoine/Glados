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
import qualified LispCases.EdgeCases.EdgeCases as EdgeCasesTest
import qualified LispCases.ArithmeticOps.ArithmeticOps as ArithmeticOpsTest
import qualified LispCases.Comparisons.Comparisons as ComparisonsTest
import qualified LispCases.ParseErrors.ParseErrors as ParseErrorsTest
import qualified LispCases.RuntimeErrors.RuntimeErrors as RuntimeErrorsTest

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
  edgeCasesTests <- EdgeCasesTest.tests
  arithmeticOpsTests <- ArithmeticOpsTest.tests
  comparisonsTests <- ComparisonsTest.tests
  parseErrorsTests <- ParseErrorsTest.tests
  runtimeErrorsTests <- RuntimeErrorsTest.tests
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
    , edgeCasesTests
    , arithmeticOpsTests
    , comparisonsTests
    , parseErrorsTests
    , runtimeErrorsTests
    ]