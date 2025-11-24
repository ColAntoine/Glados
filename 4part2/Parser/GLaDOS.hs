{-
-- EPITECH PROJECT, 2025
-- GLaDOS Parser
-- File description:
-- Parser for GLaDOS language (C-like syntax)
-}

module Parser.GLaDOS
    ( parseGLaDOSProgram
    ) where

import Parser
import Control.Applicative ((<|>), optional, many, some)
import Data.Char (isSpace, isAlpha, isAlphaNum)
import AST.GLaDOS

parseGLaDOSProgram :: String -> Either String GLaDOSProgram
parseGLaDOSProgram input = case runParser (parseSpace *> parseProgram <* parseSpace) input of
    Just (prog, rest) | all isSpace rest -> Right prog
    _ -> Left "Parsing error"

parseProgram :: Parser GLaDOSProgram
parseProgram = many (parseSpace *> parseStatement <* parseSpace)

parseStatement :: Parser GLaDOSExpr
parseStatement = parseFunction <|> parseAssignment <|> parseExpression

parseFunction :: Parser GLaDOSExpr
parseFunction = do
    keyword "fn"
    parseSpace
    name <- parseIdentifier
    parseSpace
    char '('
    parseSpace
    params <- sepBy parseIdentifier (parseSpace *> char ',' <* parseSpace)
    parseSpace
    char ')'
    parseSpace
    char '{'
    parseSpace
    body <- parseExpression
    parseSpace
    char '}'
    return $ GLFunction name params body

parseAssignment :: Parser GLaDOSExpr
parseAssignment = do
    var <- parseIdentifier
    parseSpace
    char '='
    parseSpace
    expr <- parseExpression
    parseSpace
    char ';'
    return $ GLAssign var expr

parseExpression :: Parser GLaDOSExpr
parseExpression = parseIfExpr <|> parseOrExpr

parseIfExpr :: Parser GLaDOSExpr
parseIfExpr = do
    keyword "if"
    parseSpace
    char '('
    parseSpace
    cond <- parseExpression
    parseSpace
    char ')'
    parseSpace
    char '{'
    parseSpace
    thenExpr <- parseExpression
    parseSpace
    char '}'
    parseSpace
    keyword "else"
    parseSpace
    char '{'
    parseSpace
    elseExpr <- parseExpression
    parseSpace
    char '}'
    return $ GLIf cond thenExpr elseExpr

-- Operator precedence parsing
parseOrExpr :: Parser GLaDOSExpr
parseOrExpr = parseAndExpr

parseAndExpr :: Parser GLaDOSExpr  
parseAndExpr = parseEqualityExpr

parseEqualityExpr :: Parser GLaDOSExpr
parseEqualityExpr = do
    first <- parseRelationalExpr
    rest <- many $ do
        parseSpace
        op <- (keyword "==" *> return Eq) <|> (keyword "!=" *> return Ne)
        parseSpace
        expr <- parseRelationalExpr
        return (op, expr)
    return $ foldl (\acc (op, expr) -> GLBinOp op acc expr) first rest

parseRelationalExpr :: Parser GLaDOSExpr
parseRelationalExpr = do
    first <- parseAdditiveExpr
    rest <- many $ do
        parseSpace
        op <- (keyword "<=" *> return Le) <|> 
              (keyword ">=" *> return Ge) <|>
              (char '<' *> return Lt) <|>
              (char '>' *> return Gt)
        parseSpace
        expr <- parseAdditiveExpr
        return (op, expr)
    return $ foldl (\acc (op, expr) -> GLBinOp op acc expr) first rest

parseAdditiveExpr :: Parser GLaDOSExpr
parseAdditiveExpr = do
    first <- parseMultiplicativeExpr
    rest <- many $ do
        parseSpace
        op <- (char '+' *> return Add) <|> (char '-' *> return Sub)
        parseSpace
        expr <- parseMultiplicativeExpr
        return (op, expr)
    return $ foldl (\acc (op, expr) -> GLBinOp op acc expr) first rest

parseMultiplicativeExpr :: Parser GLaDOSExpr
parseMultiplicativeExpr = do
    first <- parseUnaryExpr
    rest <- many $ do
        parseSpace
        op <- (char '*' *> return Mul) <|> (char '/' *> return Div) <|> (char '%' *> return Mod)
        parseSpace
        expr <- parseUnaryExpr
        return (op, expr)
    return $ foldl (\acc (op, expr) -> GLBinOp op acc expr) first rest

parseUnaryExpr :: Parser GLaDOSExpr
parseUnaryExpr = 
    (char '-' *> parseSpace *> (GLUnOp Neg <$> parseUnaryExpr)) <|>
    (char '!' *> parseSpace *> (GLUnOp Not <$> parseUnaryExpr)) <|>
    parsePrimaryExpr

parsePrimaryExpr :: Parser GLaDOSExpr
parsePrimaryExpr = 
    parseParenExpr <|>
    parseNumber <|>
    parseBool <|>
    parseFunctionCall <|>
    parseVariable

parseParenExpr :: Parser GLaDOSExpr
parseParenExpr = do
    char '('
    parseSpace
    expr <- parseExpression
    parseSpace
    char ')'
    return expr

parseNumber :: Parser GLaDOSExpr
parseNumber = do
    sign <- optional (char '-')
    digits <- some (oneOf "0123456789")
    let numStr = maybe "" (:[]) sign ++ digits
    return $ GLInt (read numStr)

parseBool :: Parser GLaDOSExpr
parseBool = 
    (keyword "true" *> return (GLBool True)) <|>
    (keyword "false" *> return (GLBool False))

parseFunctionCall :: Parser GLaDOSExpr
parseFunctionCall = do
    name <- parseIdentifier
    parseSpace
    char '('
    parseSpace
    args <- sepBy parseExpression (parseSpace *> char ',' <* parseSpace)
    parseSpace
    char ')'
    return $ GLCall name args

parseVariable :: Parser GLaDOSExpr
parseVariable = GLVar <$> parseIdentifier

parseIdentifier :: Parser String
parseIdentifier = do
    first <- oneOf ('_' : ['a'..'z'] ++ ['A'..'Z'])
    rest <- many (oneOf ('_' : ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))
    return (first : rest)

keyword :: String -> Parser String
keyword kw = do
    result <- string kw
    notFollowedBy (oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']))
    return result