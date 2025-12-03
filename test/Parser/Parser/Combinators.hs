module Parser.Parser.Combinators (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec

import Parser

tests :: IO TestTree
tests = testSpec "Parser Combinators" spec

spec :: Spec
spec = do
  it "char succeeds on matching char" $ do
    runParser (char 'a') "abc" `shouldBe` Just ('a',"bc")

  it "string parses a sequence" $ do
    runParser (string "hi") "hithere" `shouldBe` Just ("hi","there")

  it "oneOf accepts any listed char" $ do
    runParser (oneOf "abc") "cat" `shouldBe` Just ('c',"at")

  it "noneOf rejects listed chars" $ do
    runParser (noneOf "abc") "zoo" `shouldBe` Just ('z',"oo")

  it "digit accepts a digit" $ do
    runParser digit "9x" `shouldBe` Just ('9',"x")

  it "parseSpace consumes whitespace" $ do
    runParser parseSpace " \t\nA" `shouldBe` Just (" \t\n","A")

  it "many and some behaviour" $ do
    runParser (some (char 'a')) "aaab" `shouldBe` Just ("aaa","b")
    runParser (many (char 'a')) "bb" `shouldBe` Just ([],"bb")

  it "sepBy1 collects items separated by sep" $ do
    -- ensure sepBy1 succeeds on repeated items separated by sep
    runParser (sepBy1 (char 'a') (char ',')) "a,a,b" `shouldSatisfy` (/= Nothing)

  it "stripPrefix and isPrefixOf behave correctly" $ do
    stripPrefix "ab" "abc" `shouldBe` Just "c"
    stripPrefix "x" "abc" `shouldBe` Nothing
    isPrefixOf "ab" "abc" `shouldBe` True
    isPrefixOf "abz" "abc" `shouldBe` False

  it "between consumes open and close" $ do
    runParser (between (char '(') (char ')') (char 'a')) "(a)rest" `shouldBe` Just ('a',"rest")

  it "anyChar returns first char" $ do
    runParser anyChar "Z" `shouldBe` Just ('Z',"")

  it "manyTill collects until end parser" $ do
    runParser (manyTill (char 'a') (char 'b')) "aaabC" `shouldBe` Just ("aaa","C")

  it "notFollowedBy succeeds only when inner parser fails" $ do
    runParser (notFollowedBy (char 'a')) "abc" `shouldBe` Nothing
    runParser (notFollowedBy (char 'b')) "abc" `shouldBe` Just ((),"abc")


