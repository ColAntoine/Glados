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