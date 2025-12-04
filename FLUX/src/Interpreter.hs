module Interpreter
  ( runProgram
  ) where

import AST
import qualified Parser as P
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Control.Monad (forM)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitWith, ExitCode(..))

data Value
  = VInt Int64
  | VBool Bool
  | VString String
  | VList [Value]
  | VTuple [Value]
  | VClosure [String] Expr Env
  | VPrim ([Value] -> IO (Either String Value))

type Env = [(String, Value)]

showValue :: Value -> String
showValue (VInt n) = show n
showValue (VBool True) = "#t"
showValue (VBool False) = "#f"
showValue (VString s) = s
showValue (VList xs) = "[" ++ inner xs ++ "]"
  where inner [] = ""
        inner (v:vs) = showValue v ++ concatMap ("," ++) (map showValue vs)
showValue (VTuple xs) = "(" ++ inner xs ++ ")"
  where inner [] = ""
        inner (v:vs) = showValue v ++ concatMap ("," ++) (map showValue vs)
showValue (VClosure _ _ _) = "#<procedure>"
showValue (VPrim _) = "#<procedure>"

-- Helpers
emptyEnv :: Env
emptyEnv = []

initialEnv :: IO Env
initialEnv = pure
  [ ("print", VPrim primPrint)
  , ("map", VPrim primMap)
  ]

primPrint :: [Value] -> IO (Either String Value)
primPrint [v] = do
  putStrLn (showValue v)
  pure (Right v)
primPrint _ = pure (Left "arity mismatch")

primMap :: [Value] -> IO (Either String Value)
-- support both orders: map(fn, list) and map(list, fn) to match pipeline desugaring
primMap [VClosure params body closEnv, VList vals] = primMap [VList vals, VClosure params body closEnv]
primMap [VList vals, VClosure params body closEnv] = case params of
  [p] -> do
    results <- forM vals $ \v -> do
      -- apply closure to v
      let callEnv = (p, v) : closEnv
      e <- evalExpr callEnv body
      case e of
        Left err -> pure (Left err)
        Right val -> pure (Right val)
    case sequence results of
      Left err -> pure (Left err)
      Right vs -> pure (Right (VList vs))
  _ -> pure (Left "arity mismatch")
primMap [VPrim f, VList vals] = do
  results <- forM vals $ \v -> do
    r <- f [v]
    case r of
      Left err -> pure (Left err)
      Right val -> pure (Right val)
  case sequence results of
    Left err -> pure (Left err)
    Right vs -> pure (Right (VList vs))
primMap [VList vals, VPrim f] = primMap [VPrim f, VList vals]
primMap _ = pure (Left "type error")

-- Evaluate an expression
evalExpr :: Env -> Expr -> IO (Either String Value)
evalExpr env (EInt n) = pure (Right (VInt n))
evalExpr env (EBool b) = pure (Right (VBool b))
evalExpr env (EString s) = pure (Right (VString s))
evalExpr env (EVar s) = case lookup s env of
  Just v -> pure (Right v)
  Nothing -> pure (Left ("variable " ++ s ++ " is not bound"))
evalExpr env (EList xs) = do
  vals <- mapM (evalExpr env) xs
  case sequence vals of
    Left err -> pure (Left err)
    Right vs -> pure (Right (VList vs))
evalExpr env (ETuple xs) = do
  vals <- mapM (evalExpr env) xs
  case sequence vals of
    Left err -> pure (Left err)
    Right vs -> pure (Right (VTuple vs))
evalExpr env (ERet e) = evalExpr env e  -- ERet evaluates expr and returns it
evalExpr env (ELam ps body) = pure (Right (VClosure ps body env))
evalExpr env (EIf c t e) = do
  rc <- evalExpr env c
  case rc of
    Left err -> pure (Left err)
    Right (VBool b) -> if b then evalExpr env t else evalExpr env e
    Right _ -> pure (Left "type error")
evalExpr env (ECall f args) = do
  rf <- evalExpr env f
  case rf of
    Left err -> pure (Left err)
    Right fv -> do
      argValsE <- mapM (evalExpr env) args
      case sequence argValsE of
        Left err -> pure (Left err)
        Right argVals -> applyValue env fv argVals
evalExpr env (EUnary op e) = do
  rv <- evalExpr env e
  case rv of
    Left err -> pure (Left err)
    Right (VInt n) -> case op of
      "-" -> pure (Right (VInt (negate n)))
      _ -> pure (Left "unknown unary op")
    Right (VBool b) -> case op of
      "!" -> pure (Right (VBool (not b)))
      _ -> pure (Left "unknown unary op")
    _ -> pure (Left "type error")
evalExpr env (EBinary op a b) = do
  ra <- evalExpr env a
  case ra of
    Left err -> pure (Left err)
    Right va -> do
      rb <- evalExpr env b
      case rb of
        Left err -> pure (Left err)
        Right vb -> evalBinary op va vb env
evalExpr env (EBlock tops me) = do
  -- Evaluate top-level forms in block, update env
  env' <- foldl applyTop (pure env) tops
  case me of
    Nothing -> pure (Right (VList []))
    Just e -> evalExpr env' e
  where
    applyTop ioenv tl = do
      e' <- ioenv
      case tl of
        TLFn name params body -> do
          let body' = P.desugarPipes body
              closure = VClosure params body' ((name, undefined) : e')
              recEnv = (name, closure) : e'
              closure' = VClosure params body' recEnv
          pure ((name, closure') : e')
        TLProc name params statements -> do
          -- procedures don't return values, store as closure that executes statements
          let procClosure = VClosure params (EBlock statements Nothing) ((name, undefined) : e')
              recEnv = (name, procClosure) : e'
              procClosure' = VClosure params (EBlock statements Nothing) recEnv
          pure ((name, procClosure') : e')
        TLLet name expr -> do
          let expr' = P.desugarPipes expr
          rv <- evalExpr e' expr'
          case rv of
            Left _ -> pure e'
            Right val -> pure ((name, val) : e')
        TLExpr ex -> do
          let ex' = P.desugarPipes ex
          _ <- evalExpr e' ex'
          pure e'

eqValue :: Value -> Value -> Bool
eqValue (VInt a) (VInt b) = a == b
eqValue (VBool a) (VBool b) = a == b
eqValue (VString a) (VString b) = a == b
eqValue (VList as) (VList bs) = length as == length bs && all (uncurry eqValue) (zip as bs)
eqValue (VTuple as) (VTuple bs) = length as == length bs && all (uncurry eqValue) (zip as bs)
eqValue _ _ = False

evalBinary :: Op -> Value -> Value -> Env -> IO (Either String Value)
evalBinary Add (VInt a) (VInt b) _ = pure $ Right (VInt (a + b))
evalBinary Sub (VInt a) (VInt b) _ = pure $ Right (VInt (a - b))
evalBinary Mul (VInt a) (VInt b) _ = pure $ Right (VInt (a * b))
evalBinary Div (VInt a) (VInt 0) _ = pure $ Left "division by zero"
evalBinary Div (VInt a) (VInt b) _ = pure $ Right (VInt (a `div` b))
evalBinary Mod (VInt a) (VInt 0) _ = pure $ Left "modulo by zero"
evalBinary Mod (VInt a) (VInt b) _ = pure $ Right (VInt (a `mod` b))
evalBinary Eq a b _ = pure $ Right (VBool (eqValue a b))
evalBinary Neq a b _ = pure $ Right (VBool (not (eqValue a b)))
evalBinary Lt (VInt a) (VInt b) _ = pure $ Right (VBool (a < b))
evalBinary Lte (VInt a) (VInt b) _ = pure $ Right (VBool (a <= b))
evalBinary Gt (VInt a) (VInt b) _ = pure $ Right (VBool (a > b))
evalBinary Gte (VInt a) (VInt b) _ = pure $ Right (VBool (a >= b))
evalBinary And (VBool a) (VBool b) _ = pure $ Right (VBool (a && b))
evalBinary Or (VBool a) (VBool b) _ = pure $ Right (VBool (a || b))
evalBinary Pipe l r env = -- should be desugared but handle if present
  case r of
    VClosure params body closEnv -> applyValue env r [l]
    VPrim f -> do
      res <- f [l]
      pure res
    _ -> pure (Left "type error")
evalBinary _ _ _ _ = pure $ Left "type error"

applyValue :: Env -> Value -> [Value] -> IO (Either String Value)
applyValue env (VClosure params body closEnv) args =
  if length params /= length args then pure (Left "arity mismatch") else do
    let frame = zip params args
        callEnv = frame ++ closEnv
    evalExpr callEnv body
applyValue env (VPrim f) args = f args
applyValue _ _ _ = pure (Left "type error")

-- Run whole program: bind functions, evaluate top-level expressions and return results (already printed by builtins)
runProgram :: Program -> IO (Either String ())
runProgram prog = do
  env0 <- initialEnv
  -- process top-level forms sequentially, updating env
  let loop env [] = pure (Right ())
      loop env (t:ts) = case t of
        TLFn name params body -> do
          -- recursive closure: closure env contains the binding itself
          -- desugar pipes in function body
          let body' = P.desugarPipes body
              recEnv = (name, closure') : env
              closure' = VClosure params body' recEnv
          loop recEnv ts
        TLProc name params statements -> do
          -- procedure: closure that executes statements
          let recEnv = (name, procClosure') : env
              procClosure' = VClosure params (EBlock statements Nothing) recEnv
          loop recEnv ts
        TLLet name expr -> do
          let expr' = P.desugarPipes expr
          rv <- evalExpr env expr'
          case rv of
            Left err -> pure (Left err)
            Right val -> loop ((name, val) : env) ts
        TLExpr ex -> do
          let ex' = P.desugarPipes ex
          rv <- evalExpr env ex'
          case rv of
            Left err -> pure (Left err)
            Right _ -> loop env ts

  loop env0 prog
