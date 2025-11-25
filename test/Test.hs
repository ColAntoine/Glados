import Test.Hspec
import System.Process (readProcess)
import Control.Exception (catch, SomeException)

main :: IO ()
main = hspec $ do
  describe "Scheme Interpreter" $ do
    it "should evaluate (if #t 1 2) to 1" $ do
      schemeCode <- readFile "test/cases/if1.scm"
      result <- catch (readProcess "glados" ["-lisp"] schemeCode) handleError
      result `shouldBe` "1\n"

handleError :: SomeException -> IO String
handleError e = return $ "Error: " ++ show e