module FluxCases.IfElse.IfElse (tests) where

import Test.Tasty
import FluxCases.TestUtils

tests :: IO TestTree
tests = return $ testGroup "If-Else Tests"
  [ testFluxFile "if true branch" "test/FluxCases/IfElse/if_true_branch.flux" "1\n"
  , testFluxFile "if false branch" "test/FluxCases/IfElse/if_false_branch.flux" "2\n"
  , testFluxFile "if comparison" "test/FluxCases/IfElse/if_comparison.flux" "100\n"
  ]
