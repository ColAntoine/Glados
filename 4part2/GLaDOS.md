# GLaDOS Language Implementation

This document contains all the code for the GLaDOS language implementation, which is not part of the Lisp interpreter.

## AST/GLaDOS.hs

```haskell
{-
-- EPITECH PROJECT, 2025
-- GLaDOS AST
-- File description:
-- AST definitions for the GLaDOS language (Part 2)
-}

module AST.GLaDOS
    ( GLaDOSExpr(..)
    , GLaDOSProgram
    , BinOp(..)
    , UnOp(..)
    ) where

-- GLaDOS language AST (more C-like syntax)
data GLaDOSExpr
    = GLInt Integer
    | GLBool Bool
    | GLVar String
    | GLBinOp BinOp GLaDOSExpr GLaDOSExpr
    | GLUnOp UnOp GLaDOSExpr
    | GLIf GLaDOSExpr GLaDOSExpr GLaDOSExpr
    | GLFunction String [String] GLaDOSExpr  -- function name, parameters, body
    | GLCall String [GLaDOSExpr]            -- function call
    | GLBlock [GLaDOSExpr]                  -- sequence of expressions
    | GLAssign String GLaDOSExpr            -- variable assignment
    deriving (Show, Eq)

data BinOp
    = Add | Sub | Mul | Div | Mod
    | Eq | Lt | Gt | Le | Ge | Ne
    deriving (Show, Eq)

data UnOp
    = Neg | Not
    deriving (Show, Eq)

type GLaDOSProgram = [GLaDOSExpr]
```

## Parser/GLaDOS.hs

```haskell
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
```

## Compiler.hs

```haskell
{-
-- EPITECH PROJECT, 2025
-- GLaDOS Compiler
-- File description:
-- Compiler from GLaDOS AST to x86_64 assembly
-}

module Compiler
    ( compileProgram
    , CompilerState(..)
    ) where

import AST.GLaDOS
import Control.Monad.State
import Data.List (intercalate)

-- Compiler state for managing labels and variables
data CompilerState = CompilerState
    { labelCounter :: Int
    , stackOffset :: Int
    , variables :: [(String, Int)]  -- variable name to stack offset mapping
    , functions :: [String]         -- defined function names
    } deriving (Show)

initialState :: CompilerState
initialState = CompilerState 0 0 [] []

type Compiler = State CompilerState

compileProgram :: GLaDOSProgram -> Either String String
compileProgram prog = Right $ evalState (compileProgram' prog) initialState

compileProgram' :: GLaDOSProgram -> Compiler String
compileProgram' exprs = do
    -- Generate assembly preamble
    let preamble = unlines
            [ ".section .data"
            , ""
            , ".section .text"
            , ".globl _start"
            , ""
            , "_start:"
            , "    # Initialize base pointer"
            , "    pushq %rbp"
            , "    movq %rsp, %rbp"
            , ""
            ]
    
    -- Compile each expression
    compiledExprs <- mapM compileExpr exprs
    
    -- Generate epilogue
    let epilogue = unlines
            [ ""
            , "    # Exit program"
            , "    movq %rax, %rdi    # Exit code in %rax"
            , "    movq $60, %rax     # sys_exit"
            , "    syscall"
            , ""
            , "# Built-in functions"
            , "print_int:"
            , "    # Simple print integer (just return for now)"
            , "    ret"
            ]
    
    return $ preamble ++ intercalate "\n" compiledExprs ++ epilogue

compileExpr :: GLaDOSExpr -> Compiler String
compileExpr (GLInt n) = return $ "    movq $" ++ show n ++ ", %rax"

compileExpr (GLBool True) = return $ "    movq $1, %rax"
compileExpr (GLBool False) = return $ "    movq $0, %rax"

compileExpr (GLVar name) = do
    state <- get
    case lookup name (variables state) of
        Just offset -> return $ "    movq " ++ show offset ++ "(%rbp), %rax"
        Nothing -> return $ "    # ERROR: Undefined variable " ++ name ++ "\n    movq $0, %rax"

compileExpr (GLBinOp op left right) = do
    leftCode <- compileExpr left
    rightCode <- compileExpr right
    case op of
        Add -> return $ unlines
            [ leftCode
            , "    pushq %rax      # Save left operand"
            , rightCode
            , "    movq %rax, %rbx # Move right to %rbx"
            , "    popq %rax       # Restore left operand"
            , "    addq %rbx, %rax # Add"
            ]
        Sub -> return $ unlines
            [ leftCode
            , "    pushq %rax      # Save left operand"
            , rightCode
            , "    movq %rax, %rbx # Move right to %rbx"
            , "    popq %rax       # Restore left operand"
            , "    subq %rbx, %rax # Subtract"
            ]
        Mul -> return $ unlines
            [ leftCode
            , "    pushq %rax      # Save left operand"
            , rightCode
            , "    movq %rax, %rbx # Move right to %rbx"
            , "    popq %rax       # Restore left operand"
            , "    imulq %rbx      # Multiply"
            ]
        Div -> return $ unlines
            [ leftCode
            , "    pushq %rax      # Save left operand"
            , rightCode
            , "    movq %rax, %rbx # Move right to %rbx"
            , "    popq %rax       # Restore left operand"
            , "    cqto            # Sign extend %rax to %rdx:%rax"
            , "    idivq %rbx      # Divide"
            ]
        Mod -> return $ unlines
            [ leftCode
            , "    pushq %rax      # Save left operand"
            , rightCode
            , "    movq %rax, %rbx # Move right to %rbx"
            , "    popq %rax       # Restore left operand"
            , "    cqto            # Sign extend %rax to %rdx:%rax"
            , "    idivq %rbx      # Divide"
            , "    movq %rdx, %rax # Move remainder to %rax"
            ]
        Lt -> do
            label1 <- newLabel
            label2 <- newLabel
            return $ unlines
                [ leftCode
                , "    pushq %rax      # Save left operand"
                , rightCode
                , "    movq %rax, %rbx # Move right to %rbx"
                , "    popq %rax       # Restore left operand"
                , "    cmpq %rbx, %rax # Compare"
                , "    jl " ++ label1 ++ "        # Jump if less"
                , "    movq $0, %rax   # False"
                , "    jmp " ++ label2
                , label1 ++ ":"
                , "    movq $1, %rax   # True"
                , label2 ++ ":"
                ]
        Eq -> do
            label1 <- newLabel
            label2 <- newLabel
            return $ unlines
                [ leftCode
                , "    pushq %rax      # Save left operand"
                , rightCode
                , "    movq %rax, %rbx # Move right to %rbx"
                , "    popq %rax       # Restore left operand"
                , "    cmpq %rbx, %rax # Compare"
                , "    je " ++ label1 ++ "         # Jump if equal"
                , "    movq $0, %rax   # False"
                , "    jmp " ++ label2
                , label1 ++ ":"
                , "    movq $1, %rax   # True"
                , label2 ++ ":"
                ]
        _ -> return $ leftCode ++ "\n" ++ rightCode ++ "\n    # TODO: Implement " ++ show op

compileExpr (GLUnOp op expr) = do
    exprCode <- compileExpr expr
    let opCode = case op of
            Neg -> "    negq %rax       # Negate"
            Not -> unlines
                [ "    cmpq $0, %rax   # Compare with 0"
                , "    sete %al        # Set if equal (logical NOT)"
                , "    movzbq %al, %rax # Zero extend to 64-bit"
                ]
    return $ exprCode ++ "\n" ++ opCode

compileExpr (GLIf cond thenExpr elseExpr) = do
    condCode <- compileExpr cond
    thenCode <- compileExpr thenExpr
    elseCode <- compileExpr elseExpr
    elseLabel <- newLabel
    endLabel <- newLabel
    
    return $ unlines
        [ condCode
        , "    cmpq $0, %rax   # Test condition"
        , "    je " ++ elseLabel ++ "      # Jump to else if false"
        , thenCode
        , "    jmp " ++ endLabel ++ "     # Jump to end"
        , elseLabel ++ ":"
        , elseCode
        , endLabel ++ ":"
        ]

compileExpr (GLAssign var expr) = do
    exprCode <- compileExpr expr
    state <- get
    case lookup var (variables state) of
        Just offset -> return $ exprCode ++ "\n    movq %rax, " ++ show offset ++ "(%rbp)"
        Nothing -> do
            -- Allocate new variable on stack
            let newOffset = stackOffset state - 8
            put state { stackOffset = newOffset, variables = (var, newOffset) : variables state }
            return $ unlines
                [ exprCode
                , "    subq $8, %rsp   # Allocate space for variable"
                , "    movq %rax, " ++ show newOffset ++ "(%rbp)"
                ]

compileExpr (GLCall "print" [arg]) = do
    argCode <- compileExpr arg
    return $ unlines
        [ argCode
        , "    # Print integer (simplified - just keep in %rax)"
        , "    # In a real implementation, this would convert to string and use write syscall"
        ]

compileExpr (GLCall fname args) = do
    argCodes <- mapM compileExpr args
    return $ unlines
        [ "    # Function call: " ++ fname
        , intercalate "\n" (reverse argCodes)  -- Args in reverse order for stack
        , "    call " ++ fname
        ]

compileExpr (GLFunction name params body) = do
    bodyCode <- compileExpr body
    state <- get
    put state { functions = name : functions state }
    
    return $ unlines
        [ name ++ ":"
        , "    pushq %rbp"
        , "    movq %rsp, %rbp"
        , bodyCode
        , "    popq %rbp"
        , "    ret"
        ]

compileExpr (GLBlock exprs) = do
    codes <- mapM compileExpr exprs
    return $ intercalate "\n" codes

newLabel :: Compiler String
newLabel = do
    state <- get
    let n = labelCounter state
    put state { labelCounter = n + 1 }
    return $ ".L" ++ show n
```

## Main.hs (GLaDOS-related parts)

The GLaDOS mode in Main.hs includes the `runGLaDOSMode` function and `compileAndExecute` function.

```haskell
runGLaDOSMode :: IO ()
runGLaDOSMode = do
    input <- hGetContents stdin
    case parseGLaDOSProgram input of
        Left err -> hPutStrLn stderr (formatErr err) >> exitWith (ExitFailure 84)
        Right ast -> do
            case compileProgram ast of
                Left err -> hPutStrLn stderr (formatErr err) >> exitWith (ExitFailure 84)
                Right asm -> do
                    -- Print the assembly code
                    putStrLn asm
                    putStrLn "======="
                    -- Compile and execute
                    result <- compileAndExecute asm
                    case result of
                        Left err -> hPutStrLn stderr (formatErr err) >> exitWith (ExitFailure 84)
                        Right exitCode -> print exitCode

compileAndExecute :: String -> IO (Either String Int)
compileAndExecute asm = do
    let asmFile = "/tmp/glados_temp.s"
    let objFile = "/tmp/glados_temp.o"
    let binFile = "/tmp/glados_temp"
    
    -- Write assembly to file
    writeFile asmFile asm
    
    -- Assemble with gas (GNU assembler)
    assembleResult <- system $ "as --64 " ++ asmFile ++ " -o " ++ objFile
    case assembleResult of
        ExitFailure _ -> return $ Left "Assembly failed"
        ExitSuccess -> do
            -- Link to create executable
            linkResult <- system $ "ld " ++ objFile ++ " -o " ++ binFile
            case linkResult of
                ExitFailure _ -> return $ Left "Linking failed"
                ExitSuccess -> do
                    -- Execute and capture exit code
                    execResult <- system binFile
                    -- Clean up temporary files
                    _ <- try (removeFile asmFile) :: IO (Either SomeException ())
                    _ <- try (removeFile objFile) :: IO (Either SomeException ())
                    _ <- try (removeFile binFile) :: IO (Either SomeException ())
                    
                    case execResult of
                        ExitFailure code -> return $ Right code
                        ExitSuccess -> return $ Right 0
```</content>
<parameter name="filePath">/home/keller/Delivery/Epitech/TEK3/Semester5/glados/4part2/GLaDOS.md