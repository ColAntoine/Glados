module LispCases.Error.Error (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec
import System.Process (readProcess)
import Control.Exception (catch, SomeException)

tests :: IO TestTree
tests = testSpec "Error" spec

spec :: Spec
spec = do
    it "should handle error.scm" $ do
        schemeCode <- readFile "test/LispCases/Error/error.scm"
        result <- catch (readProcess "glados" ["-lisp"] schemeCode) handleError
        result `shouldNotBe` ""

handleError :: SomeException -> IO String
handleError e = return $ "Error: " ++ show e
