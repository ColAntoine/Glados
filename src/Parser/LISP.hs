{-
-- EPITECH PROJECT, 2025
-- LISP Parser
-- File description:
-- LISP S-expression parser
-}

module Parser.LISP
    ( parseProgram
    ) where

import Parser (Parser(..), char, many, parseSpace, sepBy, noneOf, oneOf, some)
import Control.Applicative ((<|>), optional)
import Data.Char (isSpace)
import AST (Expr(..))

parseProgram :: String -> Either String [Expr]
parseProgram input = case runParser parseMany input of
    Just (es, rest) | all isSpace rest -> Right es
    _ -> Left "Parsing error"

parseMany = many (parseSpace *> parseExpr) <* parseSpace

parseExpr :: Parser Expr
parseExpr = parseBool <|> parseNumber <|> parseList <|> parseSymbol

parseBool :: Parser Expr
parseBool = (stringTok "#t" *> pure (EBool True)) <|> (stringTok "#f" *> pure (EBool False))
  where
    stringTok s = Parser $ \inp -> case stripPrefix s inp of
        Just rest -> Just (s, rest)
        Nothing -> Nothing
    stripPrefix [] ys = Just ys
    stripPrefix (x:xs) (y:ys)
        | x == y = stripPrefix xs ys
    stripPrefix _ _ = Nothing

parseNumber :: Parser Expr
parseNumber = do
    sign <- optional (char '-')
    ds <- some (oneOf "0123456789")
    let numStr = maybe "" (:[]) sign ++ ds
    pure (EInt (read numStr))

parseSymbol :: Parser Expr
parseSymbol = do
    first <- noneOf " \t\n\r()"
    rest <- many (noneOf " \t\n\r()")
    pure (ESymbol (first:rest))

parseList :: Parser Expr
parseList = do
    _ <- char '('
    parseSpace
    xs <- sepBy parseExpr parseSpace
    parseSpace
    _ <- char ')'
    pure (EList xs)