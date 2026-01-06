module Main where

import Test.Tasty
import qualified FluxTests

main :: IO ()
main = do
  fluxTests <- FluxTests.tests
  defaultMain $ testGroup "Glados Tests" [fluxTests]
