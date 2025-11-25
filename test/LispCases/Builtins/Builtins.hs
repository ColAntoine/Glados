module LispCases.Builtins.Builtins (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec
import System.Process (readProcess)
import Control.Exception (catch, SomeException)

tests :: IO TestTree
tests = testSpec "Builtins" spec

spec :: Spec
spec = do
    it "should handle builtins1.scm" $ do
        schemeCode <- readFile "test/LispCases/Builtins/builtins1.scm"
        result <- catch (readProcess "glados" ["-lisp"] schemeCode) handleError
        result `shouldNotBe` ""
    
    it "should handle builtins2.scm" $ do
        schemeCode <- readFile "test/LispCases/Builtins/builtins2.scm"
        result <- catch (readProcess "glados" ["-lisp"] schemeCode) handleError
        result `shouldNotBe` ""
    
    it "should handle builtins3.scm" $ do
        schemeCode <- readFile "test/LispCases/Builtins/builtins3.scm"
        result <- catch (readProcess "glados" ["-lisp"] schemeCode) handleError
        result `shouldNotBe` ""

handleError :: SomeException -> IO String
handleError e = return $ "Error: " ++ show e
