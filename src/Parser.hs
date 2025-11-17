{-
-- EPITECH PROJECT, 2025
-- Parser
-- File description:
-- Parser combinator implementation
-}

module Parser
    ( Parser(..)
    , char
    , string
    , oneOf
    , noneOf
    , digit
    , parseSpace
    , many
    , some
    , sepBy
    , sepBy1
    , stripPrefix
    , isPrefixOf
    , between
    , anyChar
    , manyTill
    , notFollowedBy
    ) where

import Control.Applicative (Alternative(..), optional)
import Data.Char (isDigit, isSpace)

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
        (x, rest) <- p input
        return (f x, rest)

instance Applicative Parser where
    pure x = Parser $ \input -> Just (x, input)
    (Parser p1) <*> (Parser p2) = Parser $ \input -> do
        (f, rest1) <- p1 input
        (x, rest2) <- p2 rest1
        return (f x, rest2)

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \input ->
        p1 input <|> p2 input

instance Monad Parser where
    return = pure
    (Parser p) >>= f = Parser $ \input -> do
        (x, rest) <- p input
        runParser (f x) rest

char :: Char -> Parser Char
char c = Parser $ \input -> case input of
    (x:xs) | x == c -> Just (c, xs)
    _ -> Nothing

string :: String -> Parser String
string "" = pure ""
string (c:cs) = (:) <$> char c <*> string cs

oneOf :: String -> Parser Char
oneOf str = Parser $ \input -> case input of
    (x:xs) | x `elem` str -> Just (x, xs)
    _ -> Nothing

noneOf :: String -> Parser Char
noneOf str = Parser $ \input -> case input of
    (x:xs) | not (x `elem` str) -> Just (x, xs)
    _ -> Nothing

digit :: Parser Char
digit = Parser $ \input -> case input of
    (x:xs) | isDigit x -> Just (x, xs)
    _ -> Nothing

parseSpace :: Parser String
parseSpace = many (oneOf " \t\n\r")

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = (p `sepBy1` sep) <|> pure []

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

stripPrefix :: String -> String -> Maybe String
stripPrefix [] ys = Just ys
stripPrefix (x:xs) (y:ys)
    | x == y = stripPrefix xs ys
stripPrefix _ _ = Nothing

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = open *> p <* close

anyChar :: Parser Char
anyChar = Parser $ \input -> case input of
    (x:xs) -> Just (x, xs)
    []     -> Nothing

manyTill :: Parser a -> Parser end -> Parser [a]
manyTill p end = go
  where
    go = (end *> pure []) <|> ((:) <$> p <*> go)

notFollowedBy :: Parser a -> Parser ()
notFollowedBy p = Parser $ \input ->
    case runParser p input of
        Just _  -> Nothing
        Nothing -> Just ((), input)
