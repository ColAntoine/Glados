{-
-- EPITECH PROJECT, 2025
-- AST
-- File description:
-- Shared AST definitions for the LISP interpreter
-}

module AST
    ( Expr(..)
    ) where

data Expr
    = EInt Integer
    | EBool Bool
    | ESymbol String
    | EList [Expr]
    deriving (Show, Eq)
