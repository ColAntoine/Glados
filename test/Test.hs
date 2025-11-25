module Main where

import Test.Tasty
import qualified LispCases.Call.Call as CallTest

main :: IO ()
main = do
  callTests <- CallTest.tests
  defaultMain $ testGroup "Glados Tests"
    [ callTests
    ]