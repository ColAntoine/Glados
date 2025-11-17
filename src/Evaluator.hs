module Evaluator
    ( Value(..)
    , Env
    , initialEnv
    , evalProgram
    , showValue
    , containsUnbound
    ) where

import Data.List (intercalate)
import Data.Char (toUpper)
import AST (Expr(..))

-- Runtime values and environment
data Value
    = VInt Integer
    | VBool Bool
    | VProc [String] Expr Env
    | VPrim ([Value] -> Either String Value)
    | VList [Value]
    | VUnbound String

type Env = [(String, Value)]

-- Public API
evalProgram :: Env -> [Expr] -> ([Value], Maybe String)
-- Evaluate a list of expressions, returning the list of values to print
-- and an optional error string. Top-level `define` forms are evaluated
-- for their side-effect (updating the environment) but their resulting
-- value is not included in the printed results (matches the examples in
-- subject.md).
evalProgram env exprs = go env [] exprs
    where
        go _ acc [] = (reverse acc, Nothing)
        go e acc (x:xs) =
                -- If this is a top-level define form, evaluate it to update the
                -- environment but do not append its value to the accumulator.
                case x of
                        EList (ESymbol "define" : _) ->
                                case eval e x of
                                        Left err -> (reverse acc, Just err)
                                        Right (_val, e') -> go e' acc xs
                        _ -> case eval e x of
                                        Left err -> (reverse acc, Just err)
                                        Right (val, e') -> go e' (val:acc) xs

initialEnv :: Env
initialEnv = primitives

showValue :: Value -> String
showValue (VInt n) = show n
showValue (VBool True) = "#t"
showValue (VBool False) = "#f"
showValue (VProc _ _ _) = "#<procedure>"
showValue (VPrim _) = "#<procedure>"
showValue (VList vs) = "(" ++ intercalate " " (map showValue vs) ++ ")"
showValue (VUnbound s) = map toUpper s

containsUnbound :: Value -> Bool
containsUnbound (VUnbound _) = True
containsUnbound (VList xs) = any containsUnbound xs
containsUnbound _ = False

-- Evaluation
eval :: Env -> Expr -> Either String (Value, Env)
eval env (EInt n) = Right (VInt n, env)
eval env (EBool b) = Right (VBool b, env)
eval env (ESymbol s) =
    case lookup s env of
        Just v  -> Right (v, env)
        Nothing -> Right (VUnbound s, env)
eval env (EList []) = Right (VList [], env)
eval env (EList (ESymbol "define" : rest)) = evalDefine env rest
eval env (EList (ESymbol "lambda" : rest)) = evalLambda env rest
eval env (EList (ESymbol "if" : cond:thn:els:[])) = do
    (cv, _) <- eval env cond
    case truthy cv of
        Left err -> Left err
        Right True -> fst <$> eval env thn >>= \v -> Right (v, env)
        Right False -> fst <$> eval env els >>= \v -> Right (v, env)
  where
    truthy (VBool b) = Right b
    truthy (VUnbound s) = Left ("variable " ++ s ++ " is not bound")
    truthy _ = Left "type error"
eval env (EList (h:args)) | isPlainList h = Right (exprToData (EList (h:args)), env)
eval env (EList (f:args)) = do
    (fv, _) <- eval env f
    argVals <- mapM (fmap fst . eval env) args
    case fv of
        VProc ps body closEnv -> applyUser env fv ps body closEnv argVals
        VPrim fn -> case fn argVals of
            Left err -> Left err
            Right v -> Right (v, env)
        VUnbound s -> Left ("variable " ++ s ++ " is not bound")
        _ -> Right (VList (fv:argVals), env)

evalDefine :: Env -> [Expr] -> Either String (Value, Env)
evalDefine env [ESymbol name, expr] = do
    (v, _) <- eval env expr
    let env' = (name, v) : env
    Right (v, env')
evalDefine env [EList (ESymbol name : params), body] = do
    paramNames <- mapM expectSymbol params
    let recEnv = (name, closure) : env
        closure = VProc paramNames body recEnv
    Right (closure, recEnv)
  where
    expectSymbol (ESymbol s) = Right s
    expectSymbol _ = Left "Invalid parameter name"
evalDefine _ _ = Left "Invalid define form"

evalLambda :: Env -> [Expr] -> Either String (Value, Env)
evalLambda env [EList params, body] = do
    paramNames <- mapM expectSymbol params
    let clos = VProc paramNames body env
    Right (clos, env)
  where
    expectSymbol (ESymbol s) = Right s
    expectSymbol _ = Left "Invalid parameter name"
evalLambda _ _ = Left "Invalid lambda form"

applyUser :: Env -> Value -> [String] -> Expr -> Env -> [Value] -> Either String (Value, Env)
applyUser env _ params body closEnv args =
    if length params /= length args then Left "arity mismatch" else do
        mapM_ ensureBound args
        let frame = zip params args
            envCall = frame ++ closEnv
        fst <$> eval envCall body >>= \v -> Right (v, env)
  where
    ensureBound (VUnbound s) = Left ("variable " ++ s ++ " is not bound")
    ensureBound _ = Right ()

primitives :: Env
primitives =
    [ ("+", prim arithPlus)
    , ("-", prim arithMinus)
    , ("*", prim arithTimes)
    , ("div", prim arithDiv)
    , ("mod", prim arithMod)
    , ("eq?", prim primEq)
    , ("=", prim primEq)
    , ("<", prim primLt)
    ]
  where prim f = VPrim f

primEq :: [Value] -> Either String Value
primEq [a,b] = do
    ea <- extract a
    eb <- extract b
    pure $ VBool (ea == eb)
  where
    extract (VInt n) = Right (Left n)
    extract (VBool b) = Right (Right b)
    extract (VUnbound s) = Left ("variable " ++ s ++ " is not bound")
    extract _ = Right (Left (-1))
primEq _ = Left "arity mismatch"

primLt :: [Value] -> Either String Value
primLt [a,b] = do
    x <- getInt a
    y <- getInt b
    pure $ VBool (x < y)
primLt _ = Left "arity mismatch"

arithPlus, arithMinus, arithTimes, arithDiv, arithMod :: [Value] -> Either String Value
arithPlus vs = VInt . sum <$> mapM getInt vs
arithMinus [] = Left "arity mismatch"
arithMinus [x] = VInt . negate <$> getInt x
arithMinus (x:xs) = do
    h <- getInt x
    rest <- mapM getInt xs
    pure $ VInt (foldl (-) h rest)
arithTimes vs = VInt . product <$> mapM getInt vs
arithDiv [a,b] = do x <- getInt a; y <- getInt b; if y == 0 then Left "division by zero" else pure (VInt (x `div` y))
arithDiv _ = Left "arity mismatch"
arithMod [a,b] = do x <- getInt a; y <- getInt b; if y == 0 then Left "modulo by zero" else pure (VInt (x `mod` y))
arithMod _ = Left "arity mismatch"

getInt :: Value -> Either String Integer
getInt (VInt n) = Right n
getInt (VUnbound s) = Left ("variable " ++ s ++ " is not bound")
getInt _ = Left "type error"

-- Helpers for printing data lists
isPlainList :: Expr -> Bool
isPlainList (EList (ESymbol "lambda" : _)) = False
isPlainList (EList _) = True
isPlainList _ = False

exprToData :: Expr -> Value
exprToData (EInt n) = VInt n
exprToData (EBool b) = VBool b
exprToData (ESymbol s) = VUnbound s
exprToData (EList xs) = VList (map exprToData xs)
