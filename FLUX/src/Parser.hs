{-# LANGUAGE OverloadedStrings #-}
module Parser
    ( parseProgram
    , desugarPipes
    ) where

import qualified Data.Text as T
import Data.Void
import Data.Int (Int64)
import Data.Functor (void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L (charLiteral)
import Text.Megaparsec (manyTill)
import Data.Char (ord, isAlphaNum)
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import AST

type Parser = Parsec Void T.Text

-- Space consumer: skips spaces, tabs, newlines, and line comments
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") empty

-- Space consumer that does NOT consume newlines (for line-sensitive parsing)
scnl :: Parser ()
scnl = L.space (void $ takeWhile1P Nothing (\c -> c == ' ' || c == '\t')) (L.skipLineComment "//") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

lexemeNL :: Parser a -> Parser a
lexemeNL = L.lexeme scnl

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identifier :: Parser String
identifier = lexeme $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

integer :: Parser Int64
integer = lexeme (L.signed sc (fromIntegral <$> L.decimal))

reserved :: String -> Parser ()
reserved w = void (string (T.pack w) >> notFollowedBy alphaNumChar) >> sc

pProgram :: Parser Program
pProgram = sc *> many pTopLevel <* eof

pTopLevel :: Parser TopLevel
pTopLevel = choice [pFn, pLet, TLExpr <$> pExpr]

pFn :: Parser TopLevel
pFn = do
    reserved "fn"
    name <- identifier
    params <- parens (identifier `sepBy` symbol ",")
    -- check if it's a returning function (has =) or procedure (no =)
    hasEquals <- optional (symbol "=")
    case hasEquals of
        Just _ -> do
            -- returning function: fn name(params) = { ... } or fn name(params) = expr
            -- Block has explicit braces, expression stops at newline
            body <- pBlock <|> pExpr
            return $ TLFn name params body
        Nothing -> do
            -- procedure: fn name(params) { statements }
            _ <- symbol "{"
            statements <- many pTopLevel
            _ <- symbol "}"
            return $ TLProc name params statements

pLet :: Parser TopLevel
pLet = do
    reserved "let"
    name <- identifier
    _ <- symbol "="
    expr <- pExpr
    return $ TLLet name expr

pBlock :: Parser Expr
pBlock = do
    _ <- symbol "{"
    -- Parse top-level forms (fn/let/expr), but don't consume final expression as TLExpr
    tops <- many (try (pFn <|> pLet))
    -- Now get optional final expression
    mexpr <- optional pExpr
    _ <- symbol "}"
    return $ EBlock tops mexpr

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

pTerm :: Parser Expr
pTerm = choice
    [ EInt <$> integer
    , EBool True <$ reserved "true"
    , EBool False <$ reserved "false"
    , pString
    , try pCall
    , pIf
    , pList
    , pRet
    , try pTuple  -- must try before parens
    , EVar <$> identifier
    , parens pExpr
    , pLambda
    ]

pString :: Parser Expr
pString = do
    _ <- char '"'
    s <- manyTill L.charLiteral (char '"')
    sc
    return $ EString s

pIf :: Parser Expr
pIf = do
    reserved "if"
    cond <- pExpr
    -- if branch: try { expr } first, then full block
    thenBranch <- try (symbol "{" *> pExpr <* symbol "}") <|> pBlock
    _ <- reserved "else"
    elseBranch <- try (symbol "{" *> pExpr <* symbol "}") <|> pBlock
    return $ EIf cond thenBranch elseBranch

pList :: Parser Expr
pList = do
    _ <- symbol "["
    elems <- pExpr `sepBy` symbol ","
    _ <- symbol "]"
    return $ EList elems

pTuple :: Parser Expr
pTuple = do
    _ <- symbol "("
    first <- pExpr
    _ <- symbol ","
    rest <- pExpr `sepBy` symbol ","
    _ <- symbol ")"
    return $ ETuple (first : rest)

pRet :: Parser Expr
pRet = do
    reserved "RET"
    expr <- pExpr
    return $ ERet expr

pLambda :: Parser Expr
pLambda = do
    params <- parens (identifier `sepBy` symbol ",")
    _ <- symbol "=>"
    body <- pExpr
    return $ ELam params body

pCall :: Parser Expr
pCall = do
    name <- try $ do
        n <- takeWhile1P Nothing (\c -> isAlphaNum c || c == '_')
        -- Consume only spaces/tabs, NOT newlines
        _ <- takeWhileP Nothing (\c -> c == ' ' || c == '\t')
        -- Must be immediately followed by '(' (no newline between)
        lookAhead (char '(')
        return n
    sc  -- Now consume all whitespace including the spaces we skipped
    args <- parens (pExpr `sepBy` symbol ",")
    return $ ECall (EVar (T.unpack name)) args

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "-" (EUnary "-") , prefix "!" (EUnary "!") ]
  , [ binary "*" (EBinary Mul) , binary "/" (EBinary Div) , binary "%" (EBinary Mod) ]
  , [ binary "+" (EBinary Add) , binary "-" (EBinary Sub) ]
    , [ binary "==" (EBinary Eq) , binary "!=" (EBinary Neq) , binary "<=" (EBinary Lte)
        , binary ">=" (EBinary Gte) , binary "<" (EBinary Lt) , binary ">" (EBinary Gt) ]
  , [ binary "&&" (EBinary And) ]
  , [ binary "||" (EBinary Or) ]
  , [ binary "|>" (EBinary Pipe) ]
  ]
  where
    binary  name f = InfixL  (f <$ symbol (T.pack name))
    prefix  name f = Prefix  (f <$ symbol (T.pack name))

-- Public parse function
parseProgram :: String -> Either (ParseErrorBundle T.Text Void) Program
parseProgram input = runParser pProgram "<input>" (T.pack input)

-- Desugar pipeline operators: a |> b  =>  b(a), with tuple unpacking
desugarPipes :: Expr -> Expr
desugarPipes e = case e of
    EBinary Pipe a b ->
        let a' = desugarPipes a
            b' = desugarPipes b
        in case (a', b') of
            -- tuple |> func => func(tuple elements unpacked)
            (ETuple elems, ECall f args) -> ECall f (elems ++ args)
            (ETuple elems, _) -> ECall b' elems
            -- normal pipeline
            (_, ECall f args) -> ECall f (a' : args)
            (_, _) -> ECall b' [a']
    EBinary op l r -> EBinary op (desugarPipes l) (desugarPipes r)
    ECall f args -> ECall (desugarPipes f) (map desugarPipes args)
    ELam ps body -> ELam ps (desugarPipes body)
    EIf c t e2 -> EIf (desugarPipes c) (desugarPipes t) (desugarPipes e2)
    EBlock tops me -> EBlock tops (fmap desugarPipes me)
    EList xs -> EList (map desugarPipes xs)
    ETuple xs -> ETuple (map desugarPipes xs)
    ERet ex -> ERet (desugarPipes ex)
    other -> other
