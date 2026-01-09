{-# LANGUAGE OverloadedStrings #-}
module Compiler.Expr where

import AST
import qualified Parser as P
import Compiler.Types
import Compiler.Boxing
import Control.Monad (forM, forM_)
import Control.Monad.State
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Set as S

compileExpr :: Expr -> Compiler String
compileExpr (EInt n) = boxInt (show n)

compileExpr (EBool b) = do
  let val = if b then "1" else "0"
  r <- freshReg
  emit $ "  " ++ r ++
    " = insertvalue %Value { i64 1, i64 undef }, i64 " ++ val ++ ", 1"
  return r

compileExpr (EString s) = do
  strId <- addString s
  r1 <- freshReg
  let len = length s + 1
  emit $ "  " ++ r1 ++ " = getelementptr [" ++ show len ++ " x i8], [" ++ 
         show len ++ " x i8]* @.str." ++ show strId ++ ", i64 0, i64 0"
  boxString r1

compileExpr (EVar name) = do
  mReg <- getLocal name
  case mReg of
    Just reg -> do
      result <- freshReg
      emit $ "  " ++ result ++ " = load %Value, %Value* " ++ reg
      return result
    Nothing -> error $ "Undefined variable: " ++ name

compileExpr (EUnary "-" e) = do
  val <- compileExpr e
  raw <- unboxValue val
  negated <- freshReg
  emit $ "  " ++ negated ++ " = sub i64 0, " ++ raw
  boxInt negated

compileExpr (EUnary "!" e) = do
  val <- compileExpr e
  raw <- unboxValue val
  isZero <- freshReg
  emit $ "  " ++ isZero ++ " = icmp eq i64 " ++ raw ++ ", 0"
  boxBool isZero

compileExpr (EUnary op _) = error $ "Unknown unary operator: " ++ op

compileExpr (EBinary Pipe a b) = 
  compileExpr (P.desugarPipes (EBinary Pipe a b))

compileExpr (EBinary op a b) = do
  va <- compileExpr a
  vb <- compileExpr b
  ra <- unboxValue va
  rb <- unboxValue vb
  case op of
    Add -> do
      result <- freshReg
      emit $ "  " ++ result ++ " = add i64 " ++ ra ++ ", " ++ rb
      boxInt result
    Sub -> do
      result <- freshReg
      emit $ "  " ++ result ++ " = sub i64 " ++ ra ++ ", " ++ rb
      boxInt result
    Mul -> do
      result <- freshReg
      emit $ "  " ++ result ++ " = mul i64 " ++ ra ++ ", " ++ rb
      boxInt result
    Div -> do
      result <- freshReg
      emit $ "  " ++ result ++ " = sdiv i64 " ++ ra ++ ", " ++ rb
      boxInt result
    Mod -> do
      result <- freshReg
      emit $ "  " ++ result ++ " = srem i64 " ++ ra ++ ", " ++ rb
      boxInt result
    Eq -> do
      tagA <- getTag va
      tagB <- getTag vb
      tagEq <- freshReg
      valEq <- freshReg
      result <- freshReg
      emit $ "  " ++ tagEq ++ " = icmp eq i64 " ++ tagA ++ ", " ++ tagB
      emit $ "  " ++ valEq ++ " = icmp eq i64 " ++ ra ++ ", " ++ rb
      emit $ "  " ++ result ++ " = and i1 " ++ tagEq ++ ", " ++ valEq
      boxBool result
    Neq -> do
      tagA <- getTag va
      tagB <- getTag vb
      tagEq <- freshReg
      valEq <- freshReg
      bothEq <- freshReg
      result <- freshReg
      emit $ "  " ++ tagEq ++ " = icmp eq i64 " ++ tagA ++ ", " ++ tagB
      emit $ "  " ++ valEq ++ " = icmp eq i64 " ++ ra ++ ", " ++ rb
      emit $ "  " ++ bothEq ++ " = and i1 " ++ tagEq ++ ", " ++ valEq
      emit $ "  " ++ result ++ " = xor i1 " ++ bothEq ++ ", 1"
      boxBool result
    Lt -> do
      result <- freshReg
      emit $ "  " ++ result ++ " = icmp slt i64 " ++ ra ++ ", " ++ rb
      boxBool result
    Lte -> do
      result <- freshReg
      emit $ "  " ++ result ++ " = icmp sle i64 " ++ ra ++ ", " ++ rb
      boxBool result
    Gt -> do
      result <- freshReg
      emit $ "  " ++ result ++ " = icmp sgt i64 " ++ ra ++ ", " ++ rb
      boxBool result
    Gte -> do
      result <- freshReg
      emit $ "  " ++ result ++ " = icmp sge i64 " ++ ra ++ ", " ++ rb
      boxBool result
    And -> do
      ba <- freshReg
      bb <- freshReg
      result <- freshReg
      emit $ "  " ++ ba ++ " = icmp ne i64 " ++ ra ++ ", 0"
      emit $ "  " ++ bb ++ " = icmp ne i64 " ++ rb ++ ", 0"
      emit $ "  " ++ result ++ " = and i1 " ++ ba ++ ", " ++ bb
      boxBool result
    Or -> do
      ba <- freshReg
      bb <- freshReg
      result <- freshReg
      emit $ "  " ++ ba ++ " = icmp ne i64 " ++ ra ++ ", 0"
      emit $ "  " ++ bb ++ " = icmp ne i64 " ++ rb ++ ", 0"
      emit $ "  " ++ result ++ " = or i1 " ++ ba ++ ", " ++ bb
      boxBool result
    Pipe -> error "Pipe should have been desugared"

compileExpr (EIf cond thenE elseE) = do
  condVal <- compileExpr cond
  condRaw <- unboxValue condVal
  condBool <- freshReg
  emit $ "  " ++ condBool ++ " = icmp ne i64 " ++ condRaw ++ ", 0"
  
  thenLabel <- freshLabel "then"
  elseLabel <- freshLabel "else"
  endLabel <- freshLabel "endif"
  
  emit $ "  br i1 " ++ condBool ++ ", label %" ++ thenLabel ++
    ", label %" ++ elseLabel
  
  emitLabel thenLabel
  thenResult <- compileExpr thenE
  emit $ "  br label %" ++ endLabel
  thenEndBlock <- getCurrentBlock  -- Get actual predecessor block
  
  emitLabel elseLabel
  elseResult <- compileExpr elseE
  emit $ "  br label %" ++ endLabel
  elseEndBlock <- getCurrentBlock  -- Get actual predecessor block
  
  emitLabel endLabel
  result <- freshReg
  emit $ "  " ++ result ++ " = phi %Value [ " ++ thenResult ++
    ", %" ++ thenEndBlock ++ " ], [ " ++ elseResult ++
    ", %" ++ elseEndBlock ++ " ]"
  return result

compileExpr (ECall (EVar "print") [arg]) = compilePrint arg

compileExpr (ECall (EVar name) args) = do
  funcs <- gets csFuncNames
  if name `elem` funcs
    then compileKnownFunctionCall name args
    else compileUnknownFunctionCall name args

compileExpr (ECall funcExpr args) = do
  closureVal <- compileExpr funcExpr
  compileClosureCall closureVal args

compileExpr (ELam params body) = compileLambda params body

compileExpr (EList elems) = compileList elems

compileExpr (ETuple elems) = compileTuple elems

compileExpr (EBlock stmts mExpr) =
  forM_ stmts compileTopLevelImpl >> case mExpr of
    Just e -> compileExpr e
    Nothing -> boxInt "0"

compileExpr (ERet e) = compileExpr e

compileExpr (ESeq exprs) = do
  case exprs of
    [] -> boxInt "0"
    _ -> do
      results <- mapM compileExpr exprs
      return (last results)

compilePrint :: Expr -> Compiler String
compilePrint arg = do
  val <- compileExpr arg
  tag <- getTag val
  raw <- unboxValue val
  
  isInt <- freshReg
  isBool <- freshReg
  
  emit $ "  " ++ isInt ++ " = icmp eq i64 " ++ tag ++ ", 0"
  
  intLabel <- freshLabel "print.int"
  boolLabel <- freshLabel "print.bool"
  strLabel <- freshLabel "print.str"
  endLabel <- freshLabel "print.end"
  notIntLabel <- freshLabel "print.notint"
  
  emit $ "  br i1 " ++ isInt ++ ", label %" ++ intLabel ++
    ", label %" ++ notIntLabel
  
  emitLabel intLabel
  intFmt <- addString "%ld\n"
  fmtPtr1 <- freshReg
  emit $ "  " ++ fmtPtr1 ++ " = getelementptr [6 x i8], [6 x i8]* @.str." ++
    show intFmt ++ ", i64 0, i64 0"
  emit $ "  call i32 (i8*, ...) @printf(i8* " ++ fmtPtr1 ++
    ", i64 " ++ raw ++ ")"
  emit $ "  br label %" ++ endLabel
  
  emitLabel notIntLabel
  emit $ "  " ++ isBool ++ " = icmp eq i64 " ++ tag ++ ", 1"
  emit $ "  br i1 " ++ isBool ++ ", label %" ++ boolLabel ++
    ", label %" ++ strLabel
  
  emitLabel boolLabel
  trueStr <- addString "#t\n"
  falseStr <- addString "#f\n"
  boolCond <- freshReg
  emit $ "  " ++ boolCond ++ " = icmp ne i64 " ++ raw ++ ", 0"
  truePtr <- freshReg
  falsePtr <- freshReg
  emit $ "  " ++ truePtr ++ " = getelementptr [4 x i8], [4 x i8]* @.str." ++
    show trueStr ++ ", i64 0, i64 0"
  emit $ "  " ++ falsePtr ++ " = getelementptr [4 x i8], [4 x i8]* @.str." ++
    show falseStr ++ ", i64 0, i64 0"
  selPtr <- freshReg
  emit $ "  " ++ selPtr ++ " = select i1 " ++ boolCond ++ ", i8* " ++
    truePtr ++ ", i8* " ++ falsePtr
  emit $ "  call i32 (i8*, ...) @printf(i8* " ++ selPtr ++ ")"
  emit $ "  br label %" ++ endLabel
  
  emitLabel strLabel
  isString <- freshReg
  invalidLabel <- freshLabel "print.invalid"
  stringLabel <- freshLabel "print.string"
  emit $ "  " ++ isString ++ " = icmp eq i64 " ++ tag ++ ", 2"
  emit $ "  br i1 " ++ isString ++ ", label %" ++ stringLabel ++
    ", label %" ++ invalidLabel
  
  emitLabel stringLabel
  strPtr <- freshReg
  strFmt <- addString "%s"
  emit $ "  " ++ strPtr ++ " = inttoptr i64 " ++ raw ++ " to i8*"
  fmtPtr2 <- freshReg
  emit $ "  " ++ fmtPtr2 ++ " = getelementptr [3 x i8], [3 x i8]* @.str." ++
    show strFmt ++ ", i64 0, i64 0"
  emit $ "  call i32 (i8*, ...) @printf(i8* " ++ fmtPtr2 ++
    ", i8* " ++ strPtr ++ ")"
  emit $ "  br label %" ++ endLabel
  
  emitLabel invalidLabel
  invalidMsg <- addString "error: cannot print non-printable type\n"
  invalidPtr <- freshReg
  emit $ "  " ++ invalidPtr ++ " = getelementptr [40 x i8], [40 x i8]* @.str." ++
    show invalidMsg ++ ", i64 0, i64 0"
  emit $ "  call i32 (i8*, ...) @printf(i8* " ++ invalidPtr ++ ")"
  emit $ "  br label %" ++ endLabel
  
  emitLabel endLabel
  return val

compileKnownFunctionCall :: String -> [Expr] -> Compiler String
compileKnownFunctionCall name args = do
  argPtrs <- forM args $ \arg -> do
    val <- compileExpr arg
    ptr <- freshReg
    emit $ "  " ++ ptr ++ " = alloca %Value"
    emit $ "  store %Value " ++ val ++ ", %Value* " ++ ptr
    return ptr
  result <- freshReg
  let argList = intercalate ", " (map ("%Value* " ++) argPtrs)
  -- In CABI mode, call the internal function (prefixed with __flux_)
  cabiMode <- gets csCABIMode
  let funcName = if cabiMode then "__flux_" ++ name else name
  emit $ "  " ++ result ++ " = call %Value @" ++ funcName ++ "(" ++ argList ++ ")"
  return result

compileUnknownFunctionCall :: String -> [Expr] -> Compiler String
compileUnknownFunctionCall name args = do
  mReg <- getLocal name
  case mReg of
    Just reg -> do
      closureVal <- freshReg
      emit $ "  " ++ closureVal ++ " = load %Value, %Value* " ++ reg
      compileClosureCall closureVal args
    Nothing -> error $ "Unknown function: " ++ name

compileLambda :: [String] -> Expr -> Compiler String
compileLambda params body = do
  funcs <- gets csFuncNames
  currentLocals <- gets csLocals
  let bodyFree = freeVars body
      capturedNames = S.toList $ bodyFree S.\\ S.fromList params 
                                          S.\\ S.fromList funcs
                                          S.\\ S.fromList ["print", "map"]
      capturedVars = filter (`M.member` currentLocals) capturedNames
      numCaptured = length capturedVars
  
  closureName <- freshLabel "lambda"
  
  oldCode <- gets csCode
  oldLocals <- gets csLocals
  oldFunctions <- gets csFunctions
  
  modify $ \s -> s { csCode = [], csLocals = M.empty, csFunctions = [] }
  
  forM_ (zip [0..] capturedVars) $ \(i, name) -> do
    elemPtr <- freshReg
    emit $ "  " ++ elemPtr ++ " = getelementptr %Value, %Value* %env.ptr, i64 " ++ show i
    setLocal name elemPtr
  
  forM_ params $ \p -> setLocal p ("%" ++ p ++ ".ptr")
  
  bodyResult <- compileExpr body
  emit $ "  ret %Value " ++ bodyResult
  
  lambdaBodyCode <- gets csCode
  lambdaNestedFuncs <- gets csFunctions
  
  let paramList = intercalate ", " (["%Value* %env.ptr"] ++ ["%Value* %" ++ p ++ ".ptr" | p <- params])
      lambdaFunc = ["define %Value @" ++ closureName ++ "(" ++ paramList ++ ") {",
                    "entry:"] ++ lambdaBodyCode ++ ["}",""]
  
  modify $ \s -> s { csCode = oldCode, 
                     csLocals = oldLocals, 
                     csFunctions = oldFunctions ++ lambdaNestedFuncs ++ lambdaFunc }
  
  compileClosureCreation closureName numCaptured capturedVars params

compileClosureCreation :: String -> Int -> [String] -> [String] -> Compiler String
compileClosureCreation closureName numCaptured capturedVars params = do
  closurePtr <- freshReg
  emit $ "  " ++ closurePtr ++ " = call i8* @malloc(i64 24)"
  closureTyped <- freshReg
  emit $ "  " ++ closureTyped ++ " = bitcast i8* " ++ closurePtr ++ " to %Closure*"
  
  funcPtrField <- freshReg
  emit $ "  " ++ funcPtrField ++ " = getelementptr %Closure, %Closure* " ++ closureTyped ++ ", i32 0, i32 0"
  funcPtrRaw <- freshReg
  let funcType = "%Value (%Value*, " ++ intercalate ", " (replicate (length params) "%Value*") ++ ")*"
  emit $ "  " ++ funcPtrRaw ++ " = bitcast " ++ funcType ++ " @" ++ closureName ++ " to i8*"
  emit $ "  store i8* " ++ funcPtrRaw ++ ", i8** " ++ funcPtrField
  
  envSizeField <- freshReg
  emit $ "  " ++ envSizeField ++ " = getelementptr %Closure, %Closure* " ++ closureTyped ++ ", i32 0, i32 1"
  emit $ "  store i64 " ++ show numCaptured ++ ", i64* " ++ envSizeField
  
  envPtr <- if numCaptured > 0 then do
    envPtrRaw <- freshReg
    emit $ "  " ++ envPtrRaw ++ " = call i8* @malloc(i64 " ++ show (numCaptured * 16) ++ ")"
    envTyped <- freshReg
    emit $ "  " ++ envTyped ++ " = bitcast i8* " ++ envPtrRaw ++ " to %Value*"
    
    forM_ (zip [0..] capturedVars) $ \(i, name) -> do
      mReg <- getLocal name
      case mReg of
        Just reg -> do
          val <- freshReg
          emit $ "  " ++ val ++ " = load %Value, %Value* " ++ reg
          elemPtr <- freshReg
          emit $ "  " ++ elemPtr ++ " = getelementptr %Value, %Value* " ++ envTyped ++ ", i64 " ++ show i
          emit $ "  store %Value " ++ val ++ ", %Value* " ++ elemPtr
        Nothing -> error $ "Cannot capture undefined variable: " ++ name
    return envTyped
  else
    return "null"
  
  envPtrField <- freshReg
  emit $ "  " ++ envPtrField ++ " = getelementptr %Closure, %Closure* " ++ closureTyped ++ ", i32 0, i32 2"
  emit $ "  store %Value* " ++ envPtr ++ ", %Value** " ++ envPtrField
  
  ptrInt <- freshReg
  result <- freshReg
  emit $ "  " ++ ptrInt ++ " = ptrtoint i8* " ++ closurePtr ++ " to i64"
  emit $ "  " ++ result ++ " = insertvalue %Value { i64 5, i64 undef }, i64 " ++ ptrInt ++ ", 1"
  return result

compileArray :: Int -> [Expr] -> Compiler String
compileArray tag elems = do
  let n = length elems
  arrayPtr <- freshReg
  
  emit $ "  " ++ arrayPtr ++ " = call i8* @malloc(i64 16)"
  arrayTyped <- freshReg
  emit $ "  " ++ arrayTyped ++ " = bitcast i8* " ++ arrayPtr ++ " to %Array*"
  
  elemsTyped <- if n > 0 then do
    elemsPtr <- freshReg
    emit $ "  " ++ elemsPtr ++ " = call i8* @malloc(i64 " ++ show (n * 16) ++ ")"
    eTyped <- freshReg
    emit $ "  " ++ eTyped ++ " = bitcast i8* " ++ elemsPtr ++ " to %Value*"
    return eTyped
  else
    return "null"
  
  sizePtr <- freshReg
  emit $ "  " ++ sizePtr ++ " = getelementptr %Array, %Array* " ++ arrayTyped ++ ", i32 0, i32 0"
  emit $ "  store i64 " ++ show n ++ ", i64* " ++ sizePtr
  
  dataPtr <- freshReg
  emit $ "  " ++ dataPtr ++ " = getelementptr %Array, %Array* " ++ arrayTyped ++ ", i32 0, i32 1"
  emit $ "  store %Value* " ++ elemsTyped ++ ", %Value** " ++ dataPtr
  
  forM_ (zip [0..] elems) $ \(i, elem) -> do
    val <- compileExpr elem
    elemPtr <- freshReg
    emit $ "  " ++ elemPtr ++ " = getelementptr %Value, %Value* " ++ elemsTyped ++ ", i64 " ++ show i
    emit $ "  store %Value " ++ val ++ ", %Value* " ++ elemPtr
  
  ptrInt <- freshReg
  result <- freshReg
  emit $ "  " ++ ptrInt ++ " = ptrtoint i8* " ++ arrayPtr ++ " to i64"
  emit $ "  " ++ result ++ " = insertvalue %Value { i64 " ++ show tag ++ ", i64 undef }, i64 " ++ ptrInt ++ ", 1"
  return result

compileList :: [Expr] -> Compiler String
compileList = compileArray 3

compileTuple :: [Expr] -> Compiler String
compileTuple = compileArray 4

compileClosureCall :: String -> [Expr] -> Compiler String
compileClosureCall closureVal args = do
  closureTag <- getTag closureVal
  closurePtrInt <- unboxValue closureVal
  
  -- Verify that the value is actually a closure (tag 5)
  isClosure <- freshReg
  emit $ "  " ++ isClosure ++ " = icmp eq i64 " ++ closureTag ++ ", 5"
  
  closureLabel <- freshLabel "call.closure"
  errorLabel <- freshLabel "call.error"
  continueLabel <- freshLabel "call.continue"
  
  emit $ "  br i1 " ++ isClosure ++ ", label %" ++ closureLabel ++ ", label %" ++ errorLabel
  
  emitLabel errorLabel
  errMsg <- addString "error: attempt to call non-closure value\n"
  errPtr <- freshReg
  emit $ "  " ++ errPtr ++ " = getelementptr [41 x i8], [41 x i8]* @.str." ++
    show errMsg ++ ", i64 0, i64 0"
  emit $ "  call i32 (i8*, ...) @printf(i8* " ++ errPtr ++ ")"
  emit $ "  call void @exit(i32 1)"
  emit $ "  unreachable"
  
  emitLabel closureLabel
  closurePtr <- freshReg
  emit $ "  " ++ closurePtr ++ " = inttoptr i64 " ++ closurePtrInt ++ " to %Closure*"
  
  funcPtrField <- freshReg
  emit $ "  " ++ funcPtrField ++ " = getelementptr %Closure, %Closure* " ++ closurePtr ++ ", i32 0, i32 0"
  funcPtrRaw <- freshReg
  emit $ "  " ++ funcPtrRaw ++ " = load i8*, i8** " ++ funcPtrField
  
  envPtrField <- freshReg
  emit $ "  " ++ envPtrField ++ " = getelementptr %Closure, %Closure* " ++ closurePtr ++ ", i32 0, i32 2"
  envPtr <- freshReg
  emit $ "  " ++ envPtr ++ " = load %Value*, %Value** " ++ envPtrField
  
  argRegs <- forM args $ \arg -> do
    val <- compileExpr arg
    ptr <- freshReg
    emit $ "  " ++ ptr ++ " = alloca %Value"
    emit $ "  store %Value " ++ val ++ ", %Value* " ++ ptr
    return ptr
  
  let nArgs = length args
  funcTyped <- freshReg
  let argTypes = intercalate ", " (replicate (nArgs + 1) "%Value*")
  emit $ "  " ++ funcTyped ++ " = bitcast i8* " ++ funcPtrRaw ++ " to %Value (" ++ argTypes ++ ")*"
  
  result <- freshReg
  let argList = intercalate ", " (["%Value* " ++ envPtr] ++ ["%Value* " ++ r | r <- argRegs])
  emit $ "  " ++ result ++ " = call %Value " ++ funcTyped ++ "(" ++ argList ++ ")"
  return result

-- Compile top-level statements within EBlock expressions
compileTopLevelImpl :: TopLevel -> Compiler ()
compileTopLevelImpl (TLLet name expr) = do
  let expr' = P.desugarPipes expr
  val <- compileExpr expr'
  ptr <- freshReg
  emit $ "  " ++ ptr ++ " = alloca %Value"
  emit $ "  store %Value " ++ val ++ ", %Value* " ++ ptr
  setLocal name ptr

compileTopLevelImpl (TLFn name params body) = do
  modify $ \s -> s { csFuncNames = name : csFuncNames s }
  
  oldCode <- gets csCode
  oldLocals <- gets csLocals
  oldFunctions <- gets csFunctions
  modify $ \s -> s { csCode = [], csLocals = M.empty, csFunctions = [] }
  
  forM_ params $ \p -> setLocal p ("%" ++ p ++ ".ptr")
  
  let body' = P.desugarPipes body
  result <- compileExpr body'
  emit $ "  ret %Value " ++ result
  
  bodyCode <- gets csCode
  nestedFuncs <- gets csFunctions
  
  let paramList = intercalate ", " ["%Value* %" ++ p ++ ".ptr" | p <- params]
      funcDef = ["define %Value @" ++ name ++ "(" ++ paramList ++ ") {",
                 "entry:"] ++ bodyCode ++ ["}", ""]
  
  modify $ \s -> s { csCode = oldCode, 
                     csLocals = oldLocals, 
                     csFunctions = oldFunctions ++ nestedFuncs ++ funcDef }

compileTopLevelImpl (TLProc name params stmts) = do
  modify $ \s -> s { csFuncNames = name : csFuncNames s }
  
  oldCode <- gets csCode
  oldLocals <- gets csLocals
  oldFunctions <- gets csFunctions
  modify $ \s -> s { csCode = [], csLocals = M.empty, csFunctions = [] }
  
  forM_ params $ \p -> setLocal p ("%" ++ p ++ ".ptr")
  
  forM_ stmts compileTopLevelImpl
  
  result <- boxInt "0"
  emit $ "  ret %Value " ++ result
  
  bodyCode <- gets csCode
  nestedFuncs <- gets csFunctions
  
  let paramList = intercalate ", " ["%Value* %" ++ p ++ ".ptr" | p <- params]
      funcDef = ["define %Value @" ++ name ++ "(" ++ paramList ++ ") {",
                 "entry:"] ++ bodyCode ++ ["}", ""]
  
  modify $ \s -> s { csCode = oldCode, 
                     csLocals = oldLocals, 
                     csFunctions = oldFunctions ++ nestedFuncs ++ funcDef }

compileTopLevelImpl (TLExpr expr) = do
  let expr' = P.desugarPipes expr
  _ <- compileExpr expr'
  return ()

compileTopLevelImpl (TLImport _ _) = return ()
