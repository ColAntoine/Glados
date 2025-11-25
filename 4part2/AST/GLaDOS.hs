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