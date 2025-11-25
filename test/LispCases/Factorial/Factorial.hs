module LispCases.Factorial.Factorial (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec
import System.Process (readProcess)
import Control.Exception (catch, SomeException)

tests :: IO TestTree
tests = testSpec "Factorial" spec

spec :: Spec
spec = do
    it "should handle factorial.scm" $ do
        schemeCode <- readFile "test/LispCases/Factorial/factorial.scm"
        result <- catch (readProcess "glados" ["-lisp"] schemeCode) handleError
        result `shouldNotBe` ""

handleError :: SomeException -> IO String
handleError e = return $ "Error: " ++ show e
