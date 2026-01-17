module Main where

import Test.Tasty
import qualified Parser.ParserCoverage as ParserCoverage
import qualified Interpreter.InterpreterCoverage as InterpreterCoverage
import qualified Compiler.CompilerCoverage as CompilerCoverage
import qualified AST.ASTCoverage as ASTCoverage

main :: IO ()
main = do
  parserTests <- ParserCoverage.tests
  interpreterTests <- InterpreterCoverage.tests
  compilerTests <- CompilerCoverage.tests
  astTests <- ASTCoverage.tests
  
  defaultMain $ testGroup "Coverage Tests"
    [ parserTests
    , interpreterTests
    , compilerTests
    , astTests
    ]
