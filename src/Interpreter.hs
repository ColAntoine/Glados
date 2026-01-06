{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- Interpreter for Flux language AST
-}

module Interpreter
  ( runProgram
  , runProgramWithPath
  , Value(..)
  , showValue
  ) where

import AST
import qualified Parser as P
import Data.Int (Int64)
import Control.Monad (forM)
import System.FilePath (takeDirectory, (</>))
import System.Directory (doesFileExist)
import qualified Data.Set as Set

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
        inner (v:vs) = showValue v ++ concatMap (("," ++) . showValue) vs
showValue (VTuple xs) = "(" ++ inner xs ++ ")"
  where inner [] = ""
        inner (v:vs) = showValue v ++ concatMap (("," ++) . showValue) vs
showValue (VClosure _ _ _) = "#<procedure>"
showValue (VPrim _) = "#<procedure>"

initialEnv :: IO Env
initialEnv = pure
  [ ("print", VPrim primPrint)
  , ("map", VPrim primMap)
  ]

primPrint :: [Value] -> IO (Either String Value)
primPrint [v] = putStrLn (showValue v) >> pure (Right v)
primPrint _ = pure (Left "arity mismatch")

primMap :: [Value] -> IO (Either String Value)
primMap [VClosure params body closEnv, VList vals] = primMap [VList vals, VClosure params body closEnv]
primMap [VList vals, VClosure params body closEnv] = case params of
  [p] -> do
    results <- forM vals $ \v ->
      let callEnv = (p, v) : closEnv
      in evalExpr callEnv body
    case sequence results of
      Left err -> pure (Left err)
      Right vs -> pure (Right (VList vs))
  _ -> pure (Left "arity mismatch")
primMap [VPrim f, VList vals] = do
  results <- forM vals $ \v -> f [v]
  case sequence results of
    Left err -> pure (Left err)
    Right vs -> pure (Right (VList vs))
primMap [VList vals, VPrim f] = primMap [VPrim f, VList vals]
primMap _ = pure (Left "type error")

evalExpr :: Env -> Expr -> IO (Either String Value)
evalExpr _ (EInt n) = pure (Right (VInt n))
evalExpr _ (EBool b) = pure (Right (VBool b))
evalExpr _ (EString s) = pure (Right (VString s))
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
evalExpr env (ERet e) = evalExpr env e
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
        Right argVals -> applyValue fv argVals
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
        Right vb -> evalBinary op va vb
evalExpr env (EBlock tops me) = do
  env' <- foldl applyTop (pure env) tops
  case me of
    Nothing -> pure (Right (VList []))
    Just ex -> evalExpr env' ex
  where
    applyTop ioenv tl = do
      e' <- ioenv
      case tl of
        TLImport _ _ -> pure e'  -- Imports are handled at compile time
        TLFn name params body ->
          let body' = P.desugarPipes body
              recEnv = (name, closure') : e'
              closure' = VClosure params body' recEnv
          in pure ((name, closure') : e')
        TLProc name params statements ->
          let recEnv = (name, procClosure') : e'
              procClosure' = VClosure params (EBlock statements Nothing) recEnv
          in pure ((name, procClosure') : e')
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

evalBinary :: Op -> Value -> Value -> IO (Either String Value)
evalBinary Add (VInt a) (VInt b) = pure $ Right (VInt (a + b))
evalBinary Sub (VInt a) (VInt b) = pure $ Right (VInt (a - b))
evalBinary Mul (VInt a) (VInt b) = pure $ Right (VInt (a * b))
evalBinary Div (VInt _) (VInt 0) = pure $ Left "division by zero"
evalBinary Div (VInt a) (VInt b) = pure $ Right (VInt (a `div` b))
evalBinary Mod (VInt _) (VInt 0) = pure $ Left "modulo by zero"
evalBinary Mod (VInt a) (VInt b) = pure $ Right (VInt (a `mod` b))
evalBinary Eq a b = pure $ Right (VBool (eqValue a b))
evalBinary Neq a b = pure $ Right (VBool (not (eqValue a b)))
evalBinary Lt (VInt a) (VInt b) = pure $ Right (VBool (a < b))
evalBinary Lte (VInt a) (VInt b) = pure $ Right (VBool (a <= b))
evalBinary Gt (VInt a) (VInt b) = pure $ Right (VBool (a > b))
evalBinary Gte (VInt a) (VInt b) = pure $ Right (VBool (a >= b))
evalBinary And (VBool a) (VBool b) = pure $ Right (VBool (a && b))
evalBinary Or (VBool a) (VBool b) = pure $ Right (VBool (a || b))
evalBinary Pipe l r = case r of
    VClosure _ _ _ -> applyValue r [l]
    VPrim f -> f [l]
    _ -> pure (Left "type error")
evalBinary _ _ _ = pure $ Left "type error"

applyValue :: Value -> [Value] -> IO (Either String Value)
applyValue (VClosure params body closEnv) args =
  if length params /= length args then pure (Left "arity mismatch") else
    let frame = zip params args
        callEnv = frame ++ closEnv
    in evalExpr callEnv body
applyValue (VPrim f) args = f args
applyValue _ _ = pure (Left "type error")

-- | Load a file with imports
loadFileWithImports :: FilePath -> Set.Set FilePath -> IO (Either String Program)
loadFileWithImports file loaded
    | file `Set.member` loaded = return $ Right []
    | otherwise = do
        exists <- doesFileExist file
        if not exists
            then return $ Left $ "File not found: " ++ file
            else do
                input <- readFile file
                case P.parseProgram input of
                    Left err -> return $ Left $ show err
                    Right prog -> do
                        let newLoaded = Set.insert file loaded
                        -- Process imports
                        importedProgs <- forM [path | TLImport path _ <- prog] $ \path ->
                            let baseDir = takeDirectory file
                                importPath = baseDir </> path
                            in loadFileWithImports importPath newLoaded
                        case sequence importedProgs of
                            Left err -> return $ Left err
                            Right importedProg -> return $ Right (concat importedProg ++ prog)

runProgram :: Program -> IO (Either String (Maybe Value))
runProgram prog = runProgramWithPath prog ""

-- | Run program with a base directory for resolving imports
runProgramWithPath :: Program -> FilePath -> IO (Either String (Maybe Value))
runProgramWithPath prog filePath = do
  env0 <- initialEnv
  -- Load all imports first
  loadResult <- loadAllImports prog (takeDirectory filePath) Set.empty
  fullProg <- case loadResult of
    Left err -> return $ Left err
    Right p -> return $ Right p
  case fullProg of
    Left err -> return $ Left err
    Right prg ->
      let loop env [] lastVal = pure (Right lastVal)
          loop env (t:ts) _ = case t of
            TLImport _ _ -> loop env ts Nothing  -- Already handled
            TLFn name params body ->
              let body' = P.desugarPipes body
                  recEnv = (name, closure') : env
                  closure' = VClosure params body' recEnv
              in loop recEnv ts Nothing
            TLProc name params statements ->
              let recEnv = (name, procClosure') : env
                  procClosure' = VClosure params (EBlock statements Nothing) recEnv
              in loop recEnv ts Nothing
            TLLet name expr -> do
              let expr' = P.desugarPipes expr
              rv <- evalExpr env expr'
              case rv of
                Left err -> pure (Left err)
                Right val -> loop ((name, val) : env) ts (Just val)
            TLExpr ex -> do
              let ex' = P.desugarPipes ex
              rv <- evalExpr env ex'
              case rv of
                Left err -> pure (Left err)
                Right val -> loop env ts (Just val)
      in loop env0 prg Nothing

-- | Recursively load all imports
loadAllImports :: Program -> FilePath -> Set.Set FilePath -> IO (Either String Program)
loadAllImports prog baseDir loaded = do
  let imports = [path | TLImport path _ <- prog]
  importedProgs <- forM imports $ \path ->
    let importPath = if null baseDir then path else baseDir </> path
    in loadFileWithImports importPath loaded
  case sequence importedProgs of
    Left err -> return $ Left err
    Right importedProg -> return $ Right (concat importedProg ++ prog)
