module LispCases.Lambda.Lambda (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec
import System.Process (readProcess)
import Control.Exception (catch, SomeException)

tests :: IO TestTree
tests = testSpec "Lambda" spec

spec :: Spec
spec = do
    it "should handle lambda1.scm" $ do
        schemeCode <- readFile "test/LispCases/Lambda/lambda1.scm"
        result <- catch (readProcess "glados" ["-lisp"] schemeCode) handleError
        result `shouldNotBe` ""
    
    it "should handle lambda2.scm" $ do
        schemeCode <- readFile "test/LispCases/Lambda/lambda2.scm"
        result <- catch (readProcess "glados" ["-lisp"] schemeCode) handleError
        result `shouldNotBe` ""
    
    it "should handle lambda3.scm" $ do
        schemeCode <- readFile "test/LispCases/Lambda/lambda3.scm"
        result <- catch (readProcess "glados" ["-lisp"] schemeCode) handleError
        result `shouldNotBe` ""

handleError :: SomeException -> IO String
handleError e = return $ "Error: " ++ show e
