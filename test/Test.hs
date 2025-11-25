module Main where

import Test.Tasty
import System.Environment (getArgs, withArgs)
import qualified LispCases.Call.Call as CallTest
import qualified LispCases.Builtins.Builtins as BuiltinsTest
import qualified LispCases.Error.Error as ErrorTest
import qualified LispCases.Factorial.Factorial as FactorialTest
import qualified LispCases.Foo.Foo as FooTest
import qualified LispCases.Function.Function as FunctionTest
import qualified LispCases.If.If as IfTest
import qualified LispCases.Lambda.Lambda as LambdaTest
import qualified LispCases.Superior.Superior as SuperiorTest
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

  callTests <- CallTest.tests
  builtinsTests <- BuiltinsTest.tests
  errorTests <- ErrorTest.tests
  factorialTests <- FactorialTest.tests
  fooTests <- FooTest.tests
  functionTests <- FunctionTest.tests
  ifTests <- IfTest.tests
  lambdaTests <- LambdaTest.tests
  superiorTests <- SuperiorTest.tests
  combinatorTests <- CombTest.tests
  lispParserTests <- LispParserTest.tests

  let lispGroup = testGroup "LispCases" [ callTests, builtinsTests, errorTests, factorialTests, fooTests, functionTests, ifTests, lambdaTests, superiorTests ]
      parserGroup = testGroup "Parser" [combinatorTests, lispParserTests]
      allGroup = testGroup "Glados Tests" [lispGroup, parserGroup]
      toRun = if onlyParser then parserGroup else if onlyLisp then lispGroup else allGroup

  -- forward tasty args (like --pattern) but strip our custom flags
  withArgs forwardedArgs (defaultMain toRun)