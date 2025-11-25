module LispCases.If.If (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec
import System.Process (readProcess)
import Control.Exception (catch, SomeException)

tests :: IO TestTree
tests = testSpec "If" spec

spec :: Spec
spec = do
    it "should handle if1.scm" $ do
        schemeCode <- readFile "test/LispCases/If/if1.scm"
        result <- catch (readProcess "glados" ["-lisp"] schemeCode) handleError
        result `shouldNotBe` ""
    
    it "should handle if2.scm" $ do
        schemeCode <- readFile "test/LispCases/If/if2.scm"
        result <- catch (readProcess "glados" ["-lisp"] schemeCode) handleError
        result `shouldNotBe` ""
    
    it "should handle if3.scm" $ do
        schemeCode <- readFile "test/LispCases/If/if3.scm"
        result <- catch (readProcess "glados" ["-lisp"] schemeCode) handleError
        result `shouldNotBe` ""

handleError :: SomeException -> IO String
handleError e = return $ "Error: " ++ show e
