module LispCases.Error.Error (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec
import TestHelper (evalLisp)

tests :: IO TestTree
tests = testSpec "Error" spec

spec :: Spec
spec = do
    it "should handle error.scm" $ do
        schemeCode <- readFile "test/LispCases/Error/error.scm"
        let result = evalLisp schemeCode
        -- Error cases should return Left
        case result of
          Left _ -> pure ()
          Right _ -> fail "Expected an error"
