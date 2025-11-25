module LispCases.Call.Call (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec
import System.Process (readProcess)
import Control.Exception (catch, SomeException)

tests :: IO TestTree
tests = testSpec "Call" spec

spec :: Spec
spec = do
    it "should evaluate (div 10 2) to 5" $ do
        result <- catch (readProcess "glados" ["-lisp"] "(div 10 2)") handleError
        result `shouldBe` "5\n"

handleError :: SomeException -> IO String
handleError e = return $ "Error: " ++ show e
