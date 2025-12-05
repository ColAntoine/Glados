{-
-- EPITECH PROJECT, 2025
-- Parser Combinators Test
-- File description:
-- Unit tests for Parser combinator library
-}

module ParserCombinators.ParserCombinators (tests) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec
import Parser
import Control.Applicative ((<|>), (*>), (<*))

tests :: IO TestTree
tests = testSpec "Parser Combinators" spec

spec :: Spec
spec = do
    describe "char" $ do
        it "should parse matching character" $ do
            let result = runParser (char 'a') "abc"
            result `shouldBe` Just ('a', "bc")
        
        it "should fail on non-matching character" $ do
            let result = runParser (char 'a') "bcd"
            result `shouldBe` Nothing
        
        it "should fail on empty input" $ do
            let result = runParser (char 'a') ""
            result `shouldBe` Nothing

    describe "string" $ do
        it "should parse matching string" $ do
            let result = runParser (string "hello") "hello world"
            result `shouldBe` Just ("hello", " world")
        
        it "should fail on partial match" $ do
            let result = runParser (string "hello") "hell"
            result `shouldBe` Nothing
        
        it "should fail on non-matching string" $ do
            let result = runParser (string "hello") "goodbye world"
            result `shouldBe` Nothing

    describe "digit" $ do
        it "should parse single digit" $ do
            let result = runParser digit "5abc"
            result `shouldBe` Just ('5', "abc")
        
        it "should fail on non-digit" $ do
            let result = runParser digit "abc"
            result `shouldBe` Nothing
        
        it "should parse zero" $ do
            let result = runParser digit "0xyz"
            result `shouldBe` Just ('0', "xyz")

    describe "oneOf" $ do
        it "should parse character in set" $ do
            let result = runParser (oneOf "aeiou") "apple"
            result `shouldBe` Just ('a', "pple")
        
        it "should fail on character not in set" $ do
            let result = runParser (oneOf "aeiou") "xyz"
            result `shouldBe` Nothing
        
        it "should parse any matching character from set" $ do
            let result = runParser (oneOf "xyz") "yellow"
            result `shouldBe` Just ('y', "ellow")

    describe "noneOf" $ do
        it "should parse character not in set" $ do
            let result = runParser (noneOf "aeiou") "xyz"
            result `shouldBe` Just ('x', "yz")
        
        it "should fail on character in set" $ do
            let result = runParser (noneOf "aeiou") "apple"
            result `shouldBe` Nothing
        
        it "should fail on empty input" $ do
            let result = runParser (noneOf "aeiou") ""
            result `shouldBe` Nothing

    describe "parseSpace" $ do
        it "should parse space character" $ do
            let result = runParser parseSpace " hello"
            result `shouldBe` Just (" ", "hello")
        
        it "should parse tab character" $ do
            let result = runParser parseSpace "\thello"
            result `shouldBe` Just ("\t", "hello")
        
        it "should parse newline character" $ do
            let result = runParser parseSpace "\nhello"
            result `shouldBe` Just ("\n", "hello")
        
        it "should parse multiple spaces" $ do
            let result = runParser parseSpace "   hello"
            result `shouldBe` Just ("   ", "hello")
        
        it "should fail on non-whitespace" $ do
            let result = runParser parseSpace "hello"
            result `shouldBe` Just ("", "hello")

    describe "anyChar" $ do
        it "should parse any single character" $ do
            let result = runParser anyChar "abc"
            result `shouldBe` Just ('a', "bc")
        
        it "should parse special characters" $ do
            let result = runParser anyChar "!@#"
            result `shouldBe` Just ('!', "@#")
        
        it "should fail on empty input" $ do
            let result = runParser anyChar ""
            result `shouldBe` Nothing

    describe "many" $ do
        it "should parse zero or more digits" $ do
            let result = runParser (many digit) "123abc"
            result `shouldBe` Just ("123", "abc")
        
        it "should parse zero characters if none match" $ do
            let result = runParser (many digit) "abc"
            result `shouldBe` Just ("", "abc")
        
        it "should parse all consecutive matches" $ do
            let result = runParser (many (char 'a')) "aaaabbb"
            result `shouldBe` Just ("aaaa", "bbb")

    describe "some" $ do
        it "should parse one or more digits" $ do
            let result = runParser (some digit) "123abc"
            result `shouldBe` Just ("123", "abc")
        
        it "should fail if no characters match" $ do
            let result = runParser (some digit) "abc"
            result `shouldBe` Nothing
        
        it "should parse all consecutive matches" $ do
            let result = runParser (some (char 'x')) "xxxy"
            result `shouldBe` Just ("xxx", "y")

    describe "sepBy" $ do
        it "should parse zero or more separated values" $ do
            let result = runParser (sepBy digit (char ',')) ",,"
            result `shouldBe` Just ("", ",,")
        
        it "should parse separated values" $ do
            let result = runParser (sepBy digit (char ',')) "1,2,3 end"
            result `shouldBe` Just ("123", " end")
        
        it "should stop at missing separator" $ do
            let result = runParser (sepBy digit (char ',')) "1,2;3"
            result `shouldBe` Just ("12", ";3")

    describe "sepBy1" $ do
        it "should parse one or more separated values" $ do
            let result = runParser (sepBy1 digit (char ',')) "1,2,3 end"
            result `shouldBe` Just ("123", " end")
        
        it "should fail on empty input" $ do
            let result = runParser (sepBy1 digit (char ',')) ""
            result `shouldBe` Nothing
        
        it "should parse single value without separator" $ do
            let result = runParser (sepBy1 digit (char ',')) "5,"
            result `shouldBe` Just ("5", ",")

    describe "between" $ do
        it "should parse value between delimiters" $ do
            let result = runParser (between (char '(') (char ')') (many (noneOf ")"))) "(hello)"
            result `shouldBe` Just ("hello", "")
        
        it "should fail if opening delimiter missing" $ do
            let result = runParser (between (char '(') (char ')') (many (noneOf ")"))) "hello)"
            result `shouldBe` Nothing
        
        it "should fail if closing delimiter missing" $ do
            let result = runParser (between (char '(') (char ')') (many (noneOf ")"))) "(hello"
            result `shouldBe` Nothing

    describe "manyTill" $ do
        it "should parse until terminator" $ do
            let result = runParser (manyTill anyChar (char '*')) "hello*world"
            result `shouldBe` Just ("hello", "world")
        
        it "should parse zero characters if terminator at start" $ do
            let result = runParser (manyTill anyChar (char '*')) "*world"
            result `shouldBe` Just ("", "world")
        
        it "should fail if terminator never found" $ do
            let result = runParser (manyTill anyChar (char '*')) "hello"
            result `shouldBe` Nothing

    describe "notFollowedBy" $ do
        it "should succeed if lookahead fails" $ do
            let result = runParser (notFollowedBy (char 'x') >> char 'a') "abc"
            result `shouldBe` Just ('a', "bc")
        
        it "should fail if lookahead succeeds" $ do
            let result = runParser (notFollowedBy (char 'x') >> char 'x') "xyz"
            result `shouldBe` Nothing
        
        it "should not consume input on success" $ do
            let result = runParser (notFollowedBy (string "end") >> many anyChar) "start end"
            result `shouldBe` Just ("start end", "")

    describe "stripPrefix" $ do
        it "should strip matching prefix" $ do
            let result = stripPrefix "hello" "hello world"
            result `shouldBe` Just " world"
        
        it "should fail on non-matching prefix" $ do
            let result = stripPrefix "hello" "goodbye"
            result `shouldBe` Nothing
        
        it "should strip exact match" $ do
            let result = stripPrefix "abc" "abc"
            result `shouldBe` Just ""

    describe "isPrefixOf" $ do
        it "should return True for matching prefix" $ do
            let result = isPrefixOf "hello" "hello world"
            result `shouldBe` True
        
        it "should return False for non-matching prefix" $ do
            let result = isPrefixOf "hello" "goodbye"
            result `shouldBe` False
        
        it "should return True for exact match" $ do
            let result = isPrefixOf "abc" "abc"
            result `shouldBe` True
        
        it "should return False on empty haystack" $ do
            let result = isPrefixOf "hello" ""
            result `shouldBe` False

    describe "Functor instance" $ do
        it "should map over parser result" $ do
            let p = fmap (\c -> [c, c]) (char 'a')
            let result = runParser p "abc"
            result `shouldBe` Just ("aa", "bc")
        
        it "should chain mappings" $ do
            let p = fmap length (some digit)
            let result = runParser p "12345x"
            result `shouldBe` Just (5, "x")

    describe "Applicative instance" $ do
        it "should apply parser function" $ do
            let p = (\x y -> [x, y]) <$> char 'a' <*> char 'b'
            let result = runParser p "abc"
            result `shouldBe` Just ("ab", "c")
        
        it "should fail if either parser fails" $ do
            let p = (\x y -> [x, y]) <$> char 'a' <*> char 'z'
            let result = runParser p "abc"
            result `shouldBe` Nothing

    describe "Alternative instance" $ do
        it "should try alternative on failure" $ do
            let p = char 'a' <|> char 'b'
            let result = runParser p "bcd"
            result `shouldBe` Just ('b', "cd")
        
        it "should use first parser if it succeeds" $ do
            let p = char 'a' <|> char 'a'
            let result = runParser p "abc"
            result `shouldBe` Just ('a', "bc")
        
        it "should fail if all alternatives fail" $ do
            let p = char 'x' <|> char 'y' <|> char 'z'
            let result = runParser p "abc"
            result `shouldBe` Nothing

    describe "Monad instance" $ do
        it "should chain parser operations" $ do
            let p = char '(' >> char 'x' >> char ')'
            let result = runParser p "(x)rest"
            result `shouldBe` Just (')', "rest")
        
        it "should fail if any operation fails" $ do
            let p = char '(' >> char 'x' >> char ')'
            let result = runParser p "(y)rest"
            result `shouldBe` Nothing

    describe "Complex parsing" $ do
        it "should parse comma-separated numbers" $ do
            let p = sepBy1 (some digit) (char ',')
            let result = runParser p "1,23,456end"
            result `shouldBe` Just (["1", "23", "456"], "end")
        
        it "should parse parenthesized expression" $ do
            let p = between (char '(') (char ')') (many (noneOf ")"))
            let result = runParser p "(test content)rest"
            result `shouldBe` Just ("test content", "rest")
        
        it "should parse repeated character pattern" $ do
            let p = many (some (char 'a')) <* string "end"
            let result = runParser p "aaaaend"
            result `shouldBe` Just (["aaaa"], "")
