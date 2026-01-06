{-# LANGUAGE OverloadedStrings #-}
module Compiler.Types where

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import AST

data CompilerState = CompilerState
  { csNextReg    :: Int
  , csNextLabel  :: Int
  , csNextStr    :: Int
  , csStrings    :: [(Int, String)]
  , csCode       :: [String]
  , csFunctions  :: [String]
  , csLocals     :: M.Map String String
  , csFuncNames  :: [String]
  }

type Compiler a = State CompilerState a

initialState :: CompilerState
initialState = CompilerState 0 0 0 [] [] [] M.empty []

freshReg :: Compiler String
freshReg = do
  n <- gets csNextReg
  modify $ \s -> s { csNextReg = n + 1 }
  return $ "%r" ++ show n

freshLabel :: String -> Compiler String
freshLabel prefix = do
  n <- gets csNextLabel
  modify $ \s -> s { csNextLabel = n + 1 }
  return $ prefix ++ show n

emit :: String -> Compiler ()
emit line = modify $ \s -> s { csCode = csCode s ++ [line] }

emitFunc :: String -> Compiler ()
emitFunc line = modify $ \s -> s { csFunctions = csFunctions s ++ [line] }

addString :: String -> Compiler Int
addString str = do
  n <- gets csNextStr
  modify $ \s -> s { csNextStr = n + 1, csStrings = csStrings s ++ [(n, str)] }
  return n

getLocal :: String -> Compiler (Maybe String)
getLocal name = gets (M.lookup name . csLocals)

setLocal :: String -> String -> Compiler ()
setLocal name reg =
  modify $ \s -> s { csLocals = M.insert name reg (csLocals s) }

withLocals :: M.Map String String -> Compiler a -> Compiler a
withLocals newLocals action = do
  oldLocals <- gets csLocals
  modify $ \s -> s { csLocals = M.union newLocals oldLocals }
  result <- action
  modify $ \s -> s { csLocals = oldLocals }
  return result

freeVars :: Expr -> S.Set String
freeVars (EInt _) = S.empty
freeVars (EBool _) = S.empty
freeVars (EString _) = S.empty
freeVars (EVar name) = S.singleton name
freeVars (EUnary _ e) = freeVars e
freeVars (EBinary _ a b) = S.union (freeVars a) (freeVars b)
freeVars (EIf c t e) = S.unions [freeVars c, freeVars t, freeVars e]
freeVars (ECall f args) = S.unions (freeVars f : map freeVars args)
freeVars (ELam params body) = freeVars body S.\\ S.fromList params
freeVars (EList elems) = S.unions (map freeVars elems)
freeVars (ETuple elems) = S.unions (map freeVars elems)
freeVars (EBlock stmts mExpr) = 
  let (bound, free) = foldl collectStmt (S.empty, S.empty) stmts
      exprFree = maybe S.empty freeVars mExpr
  in S.union free (exprFree S.\\ bound)
  where
    collectStmt (bound, free) tl = case tl of
      TLLet name expr -> 
        let exprFree = freeVars expr S.\\ bound
        in (S.insert name bound, S.union free exprFree)
      TLFn name _ _ -> (S.insert name bound, free)
      TLProc name _ _ -> (S.insert name bound, free)
      TLExpr expr -> (bound, S.union free (freeVars expr S.\\ bound))
      TLImport _ _ -> (bound, free)
freeVars (ERet e) = freeVars e
freeVars (ESeq exprs) = S.unions (map freeVars exprs)
