module Main where

import Test.Tasty
import System.Environment (getArgs, withArgs)
import qualified AST.AST as ASTTest
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
import qualified LispCases.DefineAndRecursion.DefineAndRecursion as DefineAndRecursionTest
import qualified LispCases.UnboundVariables.UnboundVariables as UnboundVariablesTest
import qualified LispCases.AdvancedLambda.AdvancedLambda as AdvancedLambdaTest
import qualified LispCases.DataStructures.DataStructures as DataStructuresTest
import qualified LispCases.SpecialCases.SpecialCases as SpecialCasesTest
import qualified LispCases.SymbolHandling.SymbolHandling as SymbolHandlingTest
import qualified LispCases.BooleanOps.BooleanOps as BooleanOpsTest
import qualified ParserCombinators.ParserCombinators as ParserCombinatorsTest
import qualified Parser.Parser.Combinators as CombTest
import qualified Parser.Lisp.Basic as LispParserTest

-- | Simple test-runner flag: pass --only-lisp to run only the LispCases groups.
-- Any other tasty flags (like --pattern) are forwarded.
main :: IO ()
main = do
  args <- getArgs
  let onlyLisp = "--only-lisp" `elem` args
      onlyParser = "--only-parser" `elem` args
      forwardedArgs = filter (`notElem` ["--only-lisp", "--only-parser"]) args

  astTests <- ASTTest.tests
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
  defineAndRecursionTests <- DefineAndRecursionTest.tests
  unboundVariablesTests <- UnboundVariablesTest.tests
  advancedLambdaTests <- AdvancedLambdaTest.tests
  dataStructuresTests <- DataStructuresTest.tests
  specialCasesTests <- SpecialCasesTest.tests
  symbolHandlingTests <- SymbolHandlingTest.tests
  booleanOpsTests <- BooleanOpsTest.tests
  parserCombinatorsTests <- ParserCombinatorsTest.tests
  combinatorTests <- CombTest.tests
  lispParserTests <- LispParserTest.tests

  let lispGroup = testGroup "LispCases" 
        [ astTests
        , callTests
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
        , defineAndRecursionTests
        , unboundVariablesTests
        , advancedLambdaTests
        , dataStructuresTests
        , specialCasesTests
        , symbolHandlingTests
        , booleanOpsTests
        ]
      parserGroup = testGroup "Parser" 
        [ parserCombinatorsTests
        , combinatorTests
        , lispParserTests
        ]
      allGroup = testGroup "Glados Tests" [lispGroup, parserGroup]
      toRun = if onlyParser then parserGroup else if onlyLisp then lispGroup else allGroup

  -- forward tasty args (like --pattern) but strip our custom flags
  withArgs forwardedArgs (defaultMain toRun)
