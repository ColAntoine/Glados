module AST
    ( Program
    , TopLevel(..)
    , Expr(..)
    , Op(..)
    ) where

import Data.Int (Int64)

type Program = [TopLevel]

data TopLevel
    = TLFn String [String] Expr      -- returning function: fn name(params) = body
    | TLProc String [String] [TopLevel] -- procedure: fn name(params) { statements }
    | TLLet String Expr
    | TLExpr Expr
    deriving (Eq, Show)

data Op = Add | Sub | Mul | Div | Mod
        | Eq | Neq | Lt | Lte | Gt | Gte
        | And | Or | Pipe
    deriving (Eq, Show)

data Expr
    = EInt Int64
    | EBool Bool
    | EString String
    | EVar String
    | EIf Expr Expr Expr
    | ELam [String] Expr
    | ECall Expr [Expr]
    | EUnary String Expr
    | EBinary Op Expr Expr
    | EList [Expr]
    | ETuple [Expr]
    | EBlock [TopLevel] (Maybe Expr)
    | ERet Expr
    deriving (Eq, Show)
