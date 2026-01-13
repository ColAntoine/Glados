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
      emit $ "  " ++ tagEq ++ " = icmp eq i64 " ++ tagA ++ ", " ++ tagB
      -- For strings (tag 2), use strcmp; for others, compare data
      isStringA <- freshReg
      emit $ "  " ++ isStringA ++ " = icmp eq i64 " ++ tagA ++ ", 2"
      strCmpLabel <- freshLabel "strcmp"
      normalCmpLabel <- freshLabel "normalcmp"
      mergeLabel <- freshLabel "eqmerge"
      emit $ "  br i1 " ++ isStringA ++ ", label %" ++ strCmpLabel ++ ", label %" ++ normalCmpLabel
      -- String comparison
      emit $ strCmpLabel ++ ":"
      ptrA <- freshReg
      ptrB <- freshReg
      emit $ "  " ++ ptrA ++ " = inttoptr i64 " ++ ra ++ " to i8*"
      emit $ "  " ++ ptrB ++ " = inttoptr i64 " ++ rb ++ " to i8*"
      cmpResult <- freshReg
      emit $ "  " ++ cmpResult ++ " = call i32 @strcmp(i8* " ++ ptrA ++ ", i8* " ++ ptrB ++ ")"
      strEq <- freshReg
      emit $ "  " ++ strEq ++ " = icmp eq i32 " ++ cmpResult ++ ", 0"
      strResult <- freshReg
      emit $ "  " ++ strResult ++ " = and i1 " ++ tagEq ++ ", " ++ strEq
      emit $ "  br label %" ++ mergeLabel
      -- Normal (int/bool) comparison
      emit $ normalCmpLabel ++ ":"
      valEq <- freshReg
      emit $ "  " ++ valEq ++ " = icmp eq i64 " ++ ra ++ ", " ++ rb
      normalResult <- freshReg
      emit $ "  " ++ normalResult ++ " = and i1 " ++ tagEq ++ ", " ++ valEq
      emit $ "  br label %" ++ mergeLabel
      -- Merge
      emit $ mergeLabel ++ ":"
      result <- freshReg
      emit $ "  " ++ result ++ " = phi i1 [ " ++ strResult ++ ", %" ++ strCmpLabel ++ " ], [ " ++ normalResult ++ ", %" ++ normalCmpLabel ++ " ]"
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
compileExpr (ECall (EVar "len") [arg]) = compileLen arg
compileExpr (ECall (EVar "abs") [arg]) = compileAbs arg
compileExpr (ECall (EVar "min") [a, b]) = compileMin a b
compileExpr (ECall (EVar "max") [a, b]) = compileMax a b
compileExpr (ECall (EVar "pow") [a, b]) = compilePow a b
compileExpr (ECall (EVar "isInt") [arg]) = compileIsInt arg
compileExpr (ECall (EVar "isBool") [arg]) = compileIsBool arg
compileExpr (ECall (EVar "isString") [arg]) = compileIsString arg
compileExpr (ECall (EVar "isList") [arg]) = compileIsList arg
compileExpr (ECall (EVar "head") [arg]) = compileHead arg
compileExpr (ECall (EVar "tail") [arg]) = compileTail arg
compileExpr (ECall (EVar "at") [list, idx]) = compileAt list idx
compileExpr (ECall (EVar "reverse") [arg]) = compileReverse arg
compileExpr (ECall (EVar "toUpper") [arg]) = compileToUpper arg
compileExpr (ECall (EVar "toLower") [arg]) = compileToLower arg
compileExpr (ECall (EVar "substring") [s, start, end]) = compileSubstring s start end
compileExpr (ECall (EVar "charAt") [s, idx]) = compileAt s idx
compileExpr (ECall (EVar "concat") [a, b]) = compileConcat a b
compileExpr (ECall (EVar "readFile") [path]) = compileReadFile path
compileExpr (ECall (EVar "writeFile") [path, content]) = compileWriteFile path content
compileExpr (ECall (EVar "appendFile") [path, content]) = compileAppendFile path content

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
  listLabel <- freshLabel "print.list"
  invalidLabel <- freshLabel "print.invalid"
  stringLabel <- freshLabel "print.string"
  emit $ "  " ++ isString ++ " = icmp eq i64 " ++ tag ++ ", 2"
  emit $ "  br i1 " ++ isString ++ ", label %" ++ stringLabel ++
    ", label %" ++ listLabel

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

  emitLabel listLabel
  isList <- freshReg
  listStartLabel <- freshLabel "print.list.start"
  emit $ "  " ++ isList ++ " = icmp eq i64 " ++ tag ++ ", 3"
  emit $ "  br i1 " ++ isList ++ ", label %" ++ listStartLabel ++
    ", label %" ++ invalidLabel

  emitLabel listStartLabel
  -- Print opening bracket
  openBracket <- addString "["
  openPtr <- freshReg
  emit $ "  " ++ openPtr ++ " = getelementptr [2 x i8], [2 x i8]* @.str." ++
    show openBracket ++ ", i64 0, i64 0"
  emit $ "  call i32 (i8*, ...) @printf(i8* " ++ openPtr ++ ")"
  
  -- Get array pointer
  arrayPtr <- freshReg
  emit $ "  " ++ arrayPtr ++ " = inttoptr i64 " ++ raw ++ " to %Array*"
  
  -- Get size
  sizePtr <- freshReg
  emit $ "  " ++ sizePtr ++ " = getelementptr %Array, %Array* " ++ arrayPtr ++ ", i32 0, i32 0"
  listSize <- freshReg
  emit $ "  " ++ listSize ++ " = load i64, i64* " ++ sizePtr
  
  -- Get elements pointer
  dataPtr <- freshReg
  emit $ "  " ++ dataPtr ++ " = getelementptr %Array, %Array* " ++ arrayPtr ++ ", i32 0, i32 1"
  elemsPtr <- freshReg
  emit $ "  " ++ elemsPtr ++ " = load %Value*, %Value** " ++ dataPtr
  
  -- Loop through elements
  counterPtr <- freshReg
  emit $ "  " ++ counterPtr ++ " = alloca i64"
  emit $ "  store i64 0, i64* " ++ counterPtr
  
  loopLabel <- freshLabel "print.list.loop"
  loopBodyLabel <- freshLabel "print.list.body"
  loopEndLabel <- freshLabel "print.list.end"
  
  emit $ "  br label %" ++ loopLabel
  
  emitLabel loopLabel
  counter <- freshReg
  emit $ "  " ++ counter ++ " = load i64, i64* " ++ counterPtr
  cmp <- freshReg
  emit $ "  " ++ cmp ++ " = icmp slt i64 " ++ counter ++ ", " ++ listSize
  emit $ "  br i1 " ++ cmp ++ ", label %" ++ loopBodyLabel ++ ", label %" ++ loopEndLabel
  
  emitLabel loopBodyLabel
  -- Print comma if not first element
  isFirst <- freshReg
  emit $ "  " ++ isFirst ++ " = icmp eq i64 " ++ counter ++ ", 0"
  skipCommaLabel <- freshLabel "print.list.skip.comma"
  printCommaLabel <- freshLabel "print.list.comma"
  emit $ "  br i1 " ++ isFirst ++ ", label %" ++ skipCommaLabel ++ ", label %" ++ printCommaLabel
  
  emitLabel printCommaLabel
  commaStr <- addString ","
  commaPtr <- freshReg
  emit $ "  " ++ commaPtr ++ " = getelementptr [2 x i8], [2 x i8]* @.str." ++
    show commaStr ++ ", i64 0, i64 0"
  emit $ "  call i32 (i8*, ...) @printf(i8* " ++ commaPtr ++ ")"
  emit $ "  br label %" ++ skipCommaLabel
  
  emitLabel skipCommaLabel
  -- Get element
  elemPtr <- freshReg
  emit $ "  " ++ elemPtr ++ " = getelementptr %Value, %Value* " ++ elemsPtr ++ ", i64 " ++ counter
  elem <- freshReg
  emit $ "  " ++ elem ++ " = load %Value, %Value* " ++ elemPtr
  
  -- Print element (only handle int for now)
  elemTag <- getTag elem
  elemRaw <- unboxValue elem
  elemFmt <- addString "%ld"
  elemFmtPtr <- freshReg
  emit $ "  " ++ elemFmtPtr ++ " = getelementptr [4 x i8], [4 x i8]* @.str." ++
    show elemFmt ++ ", i64 0, i64 0"
  emit $ "  call i32 (i8*, ...) @printf(i8* " ++ elemFmtPtr ++ ", i64 " ++ elemRaw ++ ")"
  
  -- Increment counter
  nextCounter <- freshReg
  emit $ "  " ++ nextCounter ++ " = add i64 " ++ counter ++ ", 1"
  emit $ "  store i64 " ++ nextCounter ++ ", i64* " ++ counterPtr
  emit $ "  br label %" ++ loopLabel
  
  emitLabel loopEndLabel
  -- Print closing bracket
  closeBracket <- addString "]\n"
  closePtr <- freshReg
  emit $ "  " ++ closePtr ++ " = getelementptr [3 x i8], [3 x i8]* @.str." ++
    show closeBracket ++ ", i64 0, i64 0"
  emit $ "  call i32 (i8*, ...) @printf(i8* " ++ closePtr ++ ")"
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

-- Simple math builtins
compileLen :: Expr -> Compiler String
compileLen arg = do
  val <- compileExpr arg
  tag <- getTag val
  raw <- unboxValue val
  
  isString <- freshReg
  emit $ "  " ++ isString ++ " = icmp eq i64 " ++ tag ++ ", 2"
  
  strLabel <- freshLabel "len.string"
  listLabel <- freshLabel "len.list"  
  endLabel <- freshLabel "len.end"
  
  emit $ "  br i1 " ++ isString ++ ", label %" ++ strLabel ++ ", label %" ++ listLabel
  
  emitLabel strLabel
  strPtr <- freshReg
  emit $ "  " ++ strPtr ++ " = inttoptr i64 " ++ raw ++ " to i8*"
  lenReg <- freshReg
  emit $ "  " ++ lenReg ++ " = call i64 @strlen(i8* " ++ strPtr ++ ")"
  boxedLen <- boxInt lenReg
  emit $ "  br label %" ++ endLabel
  
  emitLabel listLabel
  -- For lists, assume tag=3 and data is pointer to Array struct
  arrayPtr <- freshReg
  emit $ "  " ++ arrayPtr ++ " = inttoptr i64 " ++ raw ++ " to %Array*"
  sizePtr <- freshReg
  emit $ "  " ++ sizePtr ++ " = getelementptr %Array, %Array* " ++ arrayPtr ++ ", i32 0, i32 0"
  listSize <- freshReg
  emit $ "  " ++ listSize ++ " = load i64, i64* " ++ sizePtr
  boxedLen2 <- boxInt listSize
  emit $ "  br label %" ++ endLabel
  
  emitLabel endLabel
  result <- freshReg
  emit $ "  " ++ result ++ " = phi %Value [ " ++ boxedLen ++ ", %" ++ strLabel ++ " ], [ " ++ boxedLen2 ++ ", %" ++ listLabel ++ " ]"
  return result

compileAbs :: Expr -> Compiler String
compileAbs arg = do
  val <- compileExpr arg
  raw <- unboxValue val
  isNeg <- freshReg
  emit $ "  " ++ isNeg ++ " = icmp slt i64 " ++ raw ++ ", 0"
  negated <- freshReg
  emit $ "  " ++ negated ++ " = sub i64 0, " ++ raw
  absVal <- freshReg
  emit $ "  " ++ absVal ++ " = select i1 " ++ isNeg ++ ", i64 " ++ negated ++ ", i64 " ++ raw
  boxInt absVal

compileMin :: Expr -> Expr -> Compiler String
compileMin a b = do
  va <- compileExpr a
  vb <- compileExpr b
  ra <- unboxValue va
  rb <- unboxValue vb
  cmp <- freshReg
  emit $ "  " ++ cmp ++ " = icmp slt i64 " ++ ra ++ ", " ++ rb
  minVal <- freshReg
  emit $ "  " ++ minVal ++ " = select i1 " ++ cmp ++ ", i64 " ++ ra ++ ", i64 " ++ rb
  boxInt minVal

compileMax :: Expr -> Expr -> Compiler String
compileMax a b = do
  va <- compileExpr a
  vb <- compileExpr b
  ra <- unboxValue va
  rb <- unboxValue vb
  cmp <- freshReg
  emit $ "  " ++ cmp ++ " = icmp sgt i64 " ++ ra ++ ", " ++ rb
  maxVal <- freshReg
  emit $ "  " ++ maxVal ++ " = select i1 " ++ cmp ++ ", i64 " ++ ra ++ ", i64 " ++ rb
  boxInt maxVal

compilePow :: Expr -> Expr -> Compiler String
compilePow base exp = do
  vbase <- compileExpr base
  vexp <- compileExpr exp
  rbase <- unboxValue vbase
  rexp <- unboxValue vexp
  
  -- Inline power calculation with loop
  initLabel <- freshLabel "pow.init"
  loopLabel <- freshLabel "pow.loop"
  endLabel <- freshLabel "pow.end"
  
  resultPtr <- freshReg
  counterPtr <- freshReg
  emit $ "  " ++ resultPtr ++ " = alloca i64"
  emit $ "  " ++ counterPtr ++ " = alloca i64"
  emit $ "  store i64 1, i64* " ++ resultPtr
  emit $ "  store i64 0, i64* " ++ counterPtr
  emit $ "  br label %" ++ loopLabel
  
  emitLabel loopLabel
  counter <- freshReg
  result <- freshReg
  emit $ "  " ++ counter ++ " = load i64, i64* " ++ counterPtr
  emit $ "  " ++ result ++ " = load i64, i64* " ++ resultPtr
  
  cmp <- freshReg
  emit $ "  " ++ cmp ++ " = icmp slt i64 " ++ counter ++ ", " ++ rexp
  
  bodyLabel <- freshLabel "pow.body"
  emit $ "  br i1 " ++ cmp ++ ", label %" ++ bodyLabel ++ ", label %" ++ endLabel
  
  emitLabel bodyLabel
  newResult <- freshReg
  newCounter <- freshReg
  emit $ "  " ++ newResult ++ " = mul i64 " ++ result ++ ", " ++ rbase
  emit $ "  " ++ newCounter ++ " = add i64 " ++ counter ++ ", 1"
  emit $ "  store i64 " ++ newResult ++ ", i64* " ++ resultPtr
  emit $ "  store i64 " ++ newCounter ++ ", i64* " ++ counterPtr
  emit $ "  br label %" ++ loopLabel
  
  emitLabel endLabel
  finalResult <- freshReg
  emit $ "  " ++ finalResult ++ " = load i64, i64* " ++ resultPtr
  boxInt finalResult

compileIsInt :: Expr -> Compiler String
compileIsInt arg = do
  val <- compileExpr arg
  tag <- getTag val
  cmp <- freshReg
  emit $ "  " ++ cmp ++ " = icmp eq i64 " ++ tag ++ ", 0"
  boxBool cmp

compileIsBool :: Expr -> Compiler String
compileIsBool arg = do
  val <- compileExpr arg
  tag <- getTag val
  cmp <- freshReg
  emit $ "  " ++ cmp ++ " = icmp eq i64 " ++ tag ++ ", 1"
  boxBool cmp

compileIsString :: Expr -> Compiler String
compileIsString arg = do
  val <- compileExpr arg
  tag <- getTag val
  cmp <- freshReg
  emit $ "  " ++ cmp ++ " = icmp eq i64 " ++ tag ++ ", 2"
  boxBool cmp

compileIsList :: Expr -> Compiler String
compileIsList arg = do
  val <- compileExpr arg
  tag <- getTag val
  cmp <- freshReg
  emit $ "  " ++ cmp ++ " = icmp eq i64 " ++ tag ++ ", 3"
  boxBool cmp

compileToUpper :: Expr -> Compiler String
compileToUpper arg = do
  val <- compileExpr arg
  raw <- unboxValue val
  strPtr <- freshReg
  emit $ "  " ++ strPtr ++ " = inttoptr i64 " ++ raw ++ " to i8*"
  
  -- Get string length
  lenReg <- freshReg
  emit $ "  " ++ lenReg ++ " = call i64 @strlen(i8* " ++ strPtr ++ ")"
  
  -- Allocate new string
  sizeReg <- freshReg
  emit $ "  " ++ sizeReg ++ " = add i64 " ++ lenReg ++ ", 1"
  newStr <- freshReg
  emit $ "  " ++ newStr ++ " = call i8* @malloc(i64 " ++ sizeReg ++ ")"
  
  -- Loop through string and convert each character
  loopLabel <- freshLabel "toupper.loop"
  bodyLabel <- freshLabel "toupper.body"
  endLabel <- freshLabel "toupper.end"
  
  -- Initialize counter
  counterPtr <- freshReg
  emit $ "  " ++ counterPtr ++ " = alloca i64"
  emit $ "  store i64 0, i64* " ++ counterPtr
  emit $ "  br label %" ++ loopLabel
  
  emitLabel loopLabel
  counter <- freshReg
  emit $ "  " ++ counter ++ " = load i64, i64* " ++ counterPtr
  cond <- freshReg
  emit $ "  " ++ cond ++ " = icmp slt i64 " ++ counter ++ ", " ++ lenReg
  emit $ "  br i1 " ++ cond ++ ", label %" ++ bodyLabel ++ ", label %" ++ endLabel
  
  emitLabel bodyLabel
  -- Load character from source
  srcCharPtr <- freshReg
  emit $ "  " ++ srcCharPtr ++ " = getelementptr i8, i8* " ++ strPtr ++ ", i64 " ++ counter
  srcChar <- freshReg
  emit $ "  " ++ srcChar ++ " = load i8, i8* " ++ srcCharPtr
  
  -- Check if lowercase (a-z = 97-122)
  isLower1 <- freshReg
  emit $ "  " ++ isLower1 ++ " = icmp sge i8 " ++ srcChar ++ ", 97"
  isLower2 <- freshReg
  emit $ "  " ++ isLower2 ++ " = icmp sle i8 " ++ srcChar ++ ", 122"
  isLower <- freshReg
  emit $ "  " ++ isLower ++ " = and i1 " ++ isLower1 ++ ", " ++ isLower2
  
  -- Convert: subtract 32 if lowercase, else keep same
  upper <- freshReg
  emit $ "  " ++ upper ++ " = sub i8 " ++ srcChar ++ ", 32"
  finalChar <- freshReg
  emit $ "  " ++ finalChar ++ " = select i1 " ++ isLower ++ ", i8 " ++ upper ++ ", i8 " ++ srcChar
  
  -- Store in destination
  dstCharPtr <- freshReg
  emit $ "  " ++ dstCharPtr ++ " = getelementptr i8, i8* " ++ newStr ++ ", i64 " ++ counter
  emit $ "  store i8 " ++ finalChar ++ ", i8* " ++ dstCharPtr
  
  -- Increment counter
  nextCounter <- freshReg
  emit $ "  " ++ nextCounter ++ " = add i64 " ++ counter ++ ", 1"
  emit $ "  store i64 " ++ nextCounter ++ ", i64* " ++ counterPtr
  emit $ "  br label %" ++ loopLabel
  
  emitLabel endLabel
  -- Null terminate
  nullPtr <- freshReg
  emit $ "  " ++ nullPtr ++ " = getelementptr i8, i8* " ++ newStr ++ ", i64 " ++ lenReg
  emit $ "  store i8 0, i8* " ++ nullPtr
  
  boxString newStr

compileToLower :: Expr -> Compiler String
compileToLower arg = do
  val <- compileExpr arg
  raw <- unboxValue val
  strPtr <- freshReg
  emit $ "  " ++ strPtr ++ " = inttoptr i64 " ++ raw ++ " to i8*"
  
  -- Get string length
  lenReg <- freshReg
  emit $ "  " ++ lenReg ++ " = call i64 @strlen(i8* " ++ strPtr ++ ")"
  
  -- Allocate new string
  sizeReg <- freshReg
  emit $ "  " ++ sizeReg ++ " = add i64 " ++ lenReg ++ ", 1"
  newStr <- freshReg
  emit $ "  " ++ newStr ++ " = call i8* @malloc(i64 " ++ sizeReg ++ ")"
  
  -- Loop through string and convert each character
  loopLabel <- freshLabel "tolower.loop"
  bodyLabel <- freshLabel "tolower.body"
  endLabel <- freshLabel "tolower.end"
  
  -- Initialize counter
  counterPtr <- freshReg
  emit $ "  " ++ counterPtr ++ " = alloca i64"
  emit $ "  store i64 0, i64* " ++ counterPtr
  emit $ "  br label %" ++ loopLabel
  
  emitLabel loopLabel
  counter <- freshReg
  emit $ "  " ++ counter ++ " = load i64, i64* " ++ counterPtr
  cond <- freshReg
  emit $ "  " ++ cond ++ " = icmp slt i64 " ++ counter ++ ", " ++ lenReg
  emit $ "  br i1 " ++ cond ++ ", label %" ++ bodyLabel ++ ", label %" ++ endLabel
  
  emitLabel bodyLabel
  -- Load character from source
  srcCharPtr <- freshReg
  emit $ "  " ++ srcCharPtr ++ " = getelementptr i8, i8* " ++ strPtr ++ ", i64 " ++ counter
  srcChar <- freshReg
  emit $ "  " ++ srcChar ++ " = load i8, i8* " ++ srcCharPtr
  
  -- Check if uppercase (A-Z = 65-90)
  isUpper1 <- freshReg
  emit $ "  " ++ isUpper1 ++ " = icmp sge i8 " ++ srcChar ++ ", 65"
  isUpper2 <- freshReg
  emit $ "  " ++ isUpper2 ++ " = icmp sle i8 " ++ srcChar ++ ", 90"
  isUpper <- freshReg
  emit $ "  " ++ isUpper ++ " = and i1 " ++ isUpper1 ++ ", " ++ isUpper2
  
  -- Convert: add 32 if uppercase, else keep same
  lower <- freshReg
  emit $ "  " ++ lower ++ " = add i8 " ++ srcChar ++ ", 32"
  finalChar <- freshReg
  emit $ "  " ++ finalChar ++ " = select i1 " ++ isUpper ++ ", i8 " ++ lower ++ ", i8 " ++ srcChar
  
  -- Store in destination
  dstCharPtr <- freshReg
  emit $ "  " ++ dstCharPtr ++ " = getelementptr i8, i8* " ++ newStr ++ ", i64 " ++ counter
  emit $ "  store i8 " ++ finalChar ++ ", i8* " ++ dstCharPtr
  
  -- Increment counter
  nextCounter <- freshReg
  emit $ "  " ++ nextCounter ++ " = add i64 " ++ counter ++ ", 1"
  emit $ "  store i64 " ++ nextCounter ++ ", i64* " ++ counterPtr
  emit $ "  br label %" ++ loopLabel
  
  emitLabel endLabel
  -- Null terminate
  nullPtr <- freshReg
  emit $ "  " ++ nullPtr ++ " = getelementptr i8, i8* " ++ newStr ++ ", i64 " ++ lenReg
  emit $ "  store i8 0, i8* " ++ nullPtr
  
  boxString newStr

compileSubstring :: Expr -> Expr -> Expr -> Compiler String
compileSubstring s start end = do
  vs <- compileExpr s
  vstart <- compileExpr start
  vend <- compileExpr end
  raws <- unboxValue vs
  rawstart <- unboxValue vstart
  rawend <- unboxValue vend
  
  strPtr <- freshReg
  emit $ "  " ++ strPtr ++ " = inttoptr i64 " ++ raws ++ " to i8*"
  
  -- Calculate length
  subLen <- freshReg
  emit $ "  " ++ subLen ++ " = sub i64 " ++ rawend ++ ", " ++ rawstart
  
  -- Allocate new string
  allocSize <- freshReg
  emit $ "  " ++ allocSize ++ " = add i64 " ++ subLen ++ ", 1"
  newStr <- freshReg
  emit $ "  " ++ newStr ++ " = call i8* @malloc(i64 " ++ allocSize ++ ")"
  
  -- Get start pointer
  startPtr <- freshReg
  emit $ "  " ++ startPtr ++ " = getelementptr i8, i8* " ++ strPtr ++ ", i64 " ++ rawstart
  
  -- Copy substring (simplified - use memcpy)
  emit $ "  call void @llvm.memcpy.p0i8.p0i8.i64(i8* " ++ newStr ++ ", i8* " ++ startPtr ++ ", i64 " ++ subLen ++ ", i1 false)"
  
  -- Null terminate
  nullPos <- freshReg
  emit $ "  " ++ nullPos ++ " = getelementptr i8, i8* " ++ newStr ++ ", i64 " ++ subLen
  emit $ "  store i8 0, i8* " ++ nullPos
  
  boxString newStr

compileConcat :: Expr -> Expr -> Compiler String
compileConcat a b = do
  va <- compileExpr a
  vb <- compileExpr b
  rawa <- unboxValue va
  rawb <- unboxValue vb
  
  strPtrA <- freshReg
  strPtrB <- freshReg
  emit $ "  " ++ strPtrA ++ " = inttoptr i64 " ++ rawa ++ " to i8*"
  emit $ "  " ++ strPtrB ++ " = inttoptr i64 " ++ rawb ++ " to i8*"
  
  lenA <- freshReg
  lenB <- freshReg
  emit $ "  " ++ lenA ++ " = call i64 @strlen(i8* " ++ strPtrA ++ ")"
  emit $ "  " ++ lenB ++ " = call i64 @strlen(i8* " ++ strPtrB ++ ")"
  
  totalLen <- freshReg
  emit $ "  " ++ totalLen ++ " = add i64 " ++ lenA ++ ", " ++ lenB
  
  allocSize <- freshReg
  emit $ "  " ++ allocSize ++ " = add i64 " ++ totalLen ++ ", 1"
  
  newStr <- freshReg
  emit $ "  " ++ newStr ++ " = call i8* @malloc(i64 " ++ allocSize ++ ")"
  
  emit $ "  call i8* @strcpy(i8* " ++ newStr ++ ", i8* " ++ strPtrA ++ ")"
  
  destPtr <- freshReg
  emit $ "  " ++ destPtr ++ " = getelementptr i8, i8* " ++ newStr ++ ", i64 " ++ lenA
  emit $ "  call i8* @strcpy(i8* " ++ destPtr ++ ", i8* " ++ strPtrB ++ ")"
  
  boxString newStr

compileReadFile :: Expr -> Compiler String
compileReadFile pathExpr = do
  pathVal <- compileExpr pathExpr
  pathRaw <- unboxValue pathVal
  pathPtr <- freshReg
  emit $ "  " ++ pathPtr ++ " = inttoptr i64 " ++ pathRaw ++ " to i8*"
  
  -- Open file for reading
  modeStr <- addString "r"
  modePtr <- freshReg
  emit $ "  " ++ modePtr ++ " = getelementptr [2 x i8], [2 x i8]* @.str." ++ show modeStr ++ ", i64 0, i64 0"
  
  filePtr <- freshReg
  emit $ "  " ++ filePtr ++ " = call i8* @fopen(i8* " ++ pathPtr ++ ", i8* " ++ modePtr ++ ")"
  
  -- Check if file opened successfully
  isNull <- freshReg
  emit $ "  " ++ isNull ++ " = icmp eq i8* " ++ filePtr ++ ", null"
  
  errorLabel <- freshLabel "readfile.error"
  okLabel <- freshLabel "readfile.ok"
  endLabel <- freshLabel "readfile.end"
  emit $ "  br i1 " ++ isNull ++ ", label %" ++ errorLabel ++ ", label %" ++ okLabel
  
  -- Error: file not found - return empty string
  emitLabel errorLabel
  emptyStr <- addString ""
  emptyPtr <- freshReg
  emit $ "  " ++ emptyPtr ++ " = getelementptr [1 x i8], [1 x i8]* @.str." ++ show emptyStr ++ ", i64 0, i64 0"
  emptyBoxed <- boxString emptyPtr
  emit $ "  br label %" ++ endLabel
  
  -- OK: read file
  emitLabel okLabel
  -- Get file size: fseek to end
  emit $ "  call i32 @fseek(i8* " ++ filePtr ++ ", i64 0, i32 2)"
  fileSize <- freshReg
  emit $ "  " ++ fileSize ++ " = call i64 @ftell(i8* " ++ filePtr ++ ")"
  emit $ "  call i32 @fseek(i8* " ++ filePtr ++ ", i64 0, i32 0)"
  
  -- Allocate buffer (size + 1 for null terminator)
  bufSize <- freshReg
  emit $ "  " ++ bufSize ++ " = add i64 " ++ fileSize ++ ", 1"
  buffer <- freshReg
  emit $ "  " ++ buffer ++ " = call i8* @malloc(i64 " ++ bufSize ++ ")"
  
  -- Read file
  bytesRead <- freshReg
  emit $ "  " ++ bytesRead ++ " = call i64 @fread(i8* " ++ buffer ++ ", i64 1, i64 " ++ fileSize ++ ", i8* " ++ filePtr ++ ")"
  
  -- Null terminate
  nullPos <- freshReg
  emit $ "  " ++ nullPos ++ " = getelementptr i8, i8* " ++ buffer ++ ", i64 " ++ bytesRead
  emit $ "  store i8 0, i8* " ++ nullPos
  
  -- Close file
  emit $ "  call i32 @fclose(i8* " ++ filePtr ++ ")"
  
  contentBoxed <- boxString buffer
  emit $ "  br label %" ++ endLabel
  
  emitLabel endLabel
  result <- freshReg
  emit $ "  " ++ result ++ " = phi %Value [ " ++ emptyBoxed ++ ", %" ++ errorLabel ++ " ], [ " ++ contentBoxed ++ ", %" ++ okLabel ++ " ]"
  return result

compileWriteFile :: Expr -> Expr -> Compiler String
compileWriteFile pathExpr contentExpr = do
  pathVal <- compileExpr pathExpr
  pathRaw <- unboxValue pathVal
  pathPtr <- freshReg
  emit $ "  " ++ pathPtr ++ " = inttoptr i64 " ++ pathRaw ++ " to i8*"
  
  contentVal <- compileExpr contentExpr
  contentRaw <- unboxValue contentVal
  contentPtr <- freshReg
  emit $ "  " ++ contentPtr ++ " = inttoptr i64 " ++ contentRaw ++ " to i8*"
  
  -- Open file for writing
  modeStr <- addString "w"
  modePtr <- freshReg
  emit $ "  " ++ modePtr ++ " = getelementptr [2 x i8], [2 x i8]* @.str." ++ show modeStr ++ ", i64 0, i64 0"
  
  filePtr <- freshReg
  emit $ "  " ++ filePtr ++ " = call i8* @fopen(i8* " ++ pathPtr ++ ", i8* " ++ modePtr ++ ")"
  
  -- Get content length
  contentLen <- freshReg
  emit $ "  " ++ contentLen ++ " = call i64 @strlen(i8* " ++ contentPtr ++ ")"
  
  -- Write to file
  emit $ "  call i64 @fwrite(i8* " ++ contentPtr ++ ", i64 1, i64 " ++ contentLen ++ ", i8* " ++ filePtr ++ ")"
  
  -- Close file
  emit $ "  call i32 @fclose(i8* " ++ filePtr ++ ")"
  
  -- Return the content that was written
  boxString contentPtr

compileAppendFile :: Expr -> Expr -> Compiler String
compileAppendFile pathExpr contentExpr = do
  pathVal <- compileExpr pathExpr
  pathRaw <- unboxValue pathVal
  pathPtr <- freshReg
  emit $ "  " ++ pathPtr ++ " = inttoptr i64 " ++ pathRaw ++ " to i8*"
  
  contentVal <- compileExpr contentExpr
  contentRaw <- unboxValue contentVal
  contentPtr <- freshReg
  emit $ "  " ++ contentPtr ++ " = inttoptr i64 " ++ contentRaw ++ " to i8*"
  
  -- Open file for appending
  modeStr <- addString "a"
  modePtr <- freshReg
  emit $ "  " ++ modePtr ++ " = getelementptr [2 x i8], [2 x i8]* @.str." ++ show modeStr ++ ", i64 0, i64 0"
  
  filePtr <- freshReg
  emit $ "  " ++ filePtr ++ " = call i8* @fopen(i8* " ++ pathPtr ++ ", i8* " ++ modePtr ++ ")"
  
  -- Get content length
  contentLen <- freshReg
  emit $ "  " ++ contentLen ++ " = call i64 @strlen(i8* " ++ contentPtr ++ ")"
  
  -- Write to file
  emit $ "  call i64 @fwrite(i8* " ++ contentPtr ++ ", i64 1, i64 " ++ contentLen ++ ", i8* " ++ filePtr ++ ")"
  
  -- Close file
  emit $ "  call i32 @fclose(i8* " ++ filePtr ++ ")"
  
  -- Return the content that was written
  boxString contentPtr


compileHead :: Expr -> Compiler String
compileHead arg = do
  val <- compileExpr arg
  tag <- getTag val
  
  -- Check if string or list
  isString <- freshReg
  emit $ "  " ++ isString ++ " = icmp eq i64 " ++ tag ++ ", 2"
  
  strLabel <- freshLabel "head.string"
  listLabel <- freshLabel "head.list"
  endLabel <- freshLabel "head.end"
  
  emit $ "  br i1 " ++ isString ++ ", label %" ++ strLabel ++ ", label %" ++ listLabel
  
  -- String head - first character
  emitLabel strLabel
  rawStr <- unboxValue val
  strPtr <- freshReg
  emit $ "  " ++ strPtr ++ " = inttoptr i64 " ++ rawStr ++ " to i8*"
  charVal <- freshReg
  emit $ "  " ++ charVal ++ " = load i8, i8* " ++ strPtr
  
  -- Create single-char string
  newStr <- freshReg
  emit $ "  " ++ newStr ++ " = call i8* @malloc(i64 2)"
  emit $ "  store i8 " ++ charVal ++ ", i8* " ++ newStr
  nullPtr <- freshReg
  emit $ "  " ++ nullPtr ++ " = getelementptr i8, i8* " ++ newStr ++ ", i64 1"
  emit $ "  store i8 0, i8* " ++ nullPtr
  strResult <- boxString newStr
  emit $ "  br label %" ++ endLabel
  
  -- List head - first element
  emitLabel listLabel
  rawList <- unboxValue val
  arrayPtr <- freshReg
  emit $ "  " ++ arrayPtr ++ " = inttoptr i64 " ++ rawList ++ " to %Array*"
  dataPtr <- freshReg
  emit $ "  " ++ dataPtr ++ " = getelementptr %Array, %Array* " ++ arrayPtr ++ ", i32 0, i32 1"
  elemsPtr <- freshReg
  emit $ "  " ++ elemsPtr ++ " = load %Value*, %Value** " ++ dataPtr
  firstPtr <- freshReg
  emit $ "  " ++ firstPtr ++ " = getelementptr %Value, %Value* " ++ elemsPtr ++ ", i64 0"
  listResult <- freshReg
  emit $ "  " ++ listResult ++ " = load %Value, %Value* " ++ firstPtr
  emit $ "  br label %" ++ endLabel
  
  emitLabel endLabel
  result <- freshReg
  emit $ "  " ++ result ++ " = phi %Value [ " ++ strResult ++ ", %" ++ strLabel ++ " ], [ " ++ listResult ++ ", %" ++ listLabel ++ " ]"
  return result

compileTail :: Expr -> Compiler String
compileTail arg = do
  val <- compileExpr arg
  tag <- getTag val
  
  -- Check if string or list
  isString <- freshReg
  emit $ "  " ++ isString ++ " = icmp eq i64 " ++ tag ++ ", 2"
  
  strLabel <- freshLabel "tail.string"
  listLabel <- freshLabel "tail.list"
  endLabel <- freshLabel "tail.end"
  
  emit $ "  br i1 " ++ isString ++ ", label %" ++ strLabel ++ ", label %" ++ listLabel
  
  -- String tail - substring from index 1
  emitLabel strLabel
  rawStr <- unboxValue val
  strPtr <- freshReg
  emit $ "  " ++ strPtr ++ " = inttoptr i64 " ++ rawStr ++ " to i8*"
  
  -- Get length
  lenReg <- freshReg
  emit $ "  " ++ lenReg ++ " = call i64 @strlen(i8* " ++ strPtr ++ ")"
  
  -- Calculate new length
  newLen <- freshReg
  emit $ "  " ++ newLen ++ " = sub i64 " ++ lenReg ++ ", 1"
  
  -- Allocate new string
  allocSize <- freshReg
  emit $ "  " ++ allocSize ++ " = add i64 " ++ newLen ++ ", 1"
  newStr <- freshReg
  emit $ "  " ++ newStr ++ " = call i8* @malloc(i64 " ++ allocSize ++ ")"
  
  -- Copy from index 1
  srcPtr <- freshReg
  emit $ "  " ++ srcPtr ++ " = getelementptr i8, i8* " ++ strPtr ++ ", i64 1"
  emit $ "  call void @llvm.memcpy.p0i8.p0i8.i64(i8* " ++ newStr ++ ", i8* " ++ srcPtr ++ ", i64 " ++ newLen ++ ", i1 false)"
  
  -- Null terminate
  nullPos <- freshReg
  emit $ "  " ++ nullPos ++ " = getelementptr i8, i8* " ++ newStr ++ ", i64 " ++ newLen
  emit $ "  store i8 0, i8* " ++ nullPos
  
  strResult <- boxString newStr
  emit $ "  br label %" ++ endLabel
  
  -- List tail - all elements except first
  emitLabel listLabel
  rawList <- unboxValue val
  arrayPtr <- freshReg
  emit $ "  " ++ arrayPtr ++ " = inttoptr i64 " ++ rawList ++ " to %Array*"
  
  -- Get size
  sizePtr <- freshReg
  emit $ "  " ++ sizePtr ++ " = getelementptr %Array, %Array* " ++ arrayPtr ++ ", i32 0, i32 0"
  size <- freshReg
  emit $ "  " ++ size ++ " = load i64, i64* " ++ sizePtr
  
  -- Calculate new size
  newSize <- freshReg
  emit $ "  " ++ newSize ++ " = sub i64 " ++ size ++ ", 1"
  
  -- Create new array
  newArrayPtr <- freshReg
  emit $ "  " ++ newArrayPtr ++ " = call i8* @malloc(i64 16)"
  newArrayTyped <- freshReg
  emit $ "  " ++ newArrayTyped ++ " = bitcast i8* " ++ newArrayPtr ++ " to %Array*"
  
  -- Allocate new elements
  allocSize <- freshReg
  emit $ "  " ++ allocSize ++ " = mul i64 " ++ newSize ++ ", 16"
  newElemsPtr <- freshReg
  emit $ "  " ++ newElemsPtr ++ " = call i8* @malloc(i64 " ++ allocSize ++ ")"
  newElemsTyped <- freshReg
  emit $ "  " ++ newElemsTyped ++ " = bitcast i8* " ++ newElemsPtr ++ " to %Value*"
  
  -- Copy elements starting from index 1
  dataPtr <- freshReg
  emit $ "  " ++ dataPtr ++ " = getelementptr %Array, %Array* " ++ arrayPtr ++ ", i32 0, i32 1"
  oldElemsPtr <- freshReg
  emit $ "  " ++ oldElemsPtr ++ " = load %Value*, %Value** " ++ dataPtr
  
  srcPtr <- freshReg
  emit $ "  " ++ srcPtr ++ " = getelementptr %Value, %Value* " ++ oldElemsPtr ++ ", i64 1"
  
  srcI8 <- freshReg
  dstI8 <- freshReg
  emit $ "  " ++ srcI8 ++ " = bitcast %Value* " ++ srcPtr ++ " to i8*"
  emit $ "  " ++ dstI8 ++ " = bitcast %Value* " ++ newElemsTyped ++ " to i8*"
  emit $ "  call void @llvm.memcpy.p0i8.p0i8.i64(i8* " ++ dstI8 ++ ", i8* " ++ srcI8 ++ ", i64 " ++ allocSize ++ ", i1 false)"
  
  -- Store size and elements in new array
  newSizePtr <- freshReg
  emit $ "  " ++ newSizePtr ++ " = getelementptr %Array, %Array* " ++ newArrayTyped ++ ", i32 0, i32 0"
  emit $ "  store i64 " ++ newSize ++ ", i64* " ++ newSizePtr
  
  newDataPtr <- freshReg
  emit $ "  " ++ newDataPtr ++ " = getelementptr %Array, %Array* " ++ newArrayTyped ++ ", i32 0, i32 1"
  emit $ "  store %Value* " ++ newElemsTyped ++ ", %Value** " ++ newDataPtr
  
  -- Box the result
  ptrInt <- freshReg
  emit $ "  " ++ ptrInt ++ " = ptrtoint i8* " ++ newArrayPtr ++ " to i64"
  listResult <- freshReg
  emit $ "  " ++ listResult ++ " = insertvalue %Value { i64 3, i64 undef }, i64 " ++ ptrInt ++ ", 1"
  emit $ "  br label %" ++ endLabel
  
  emitLabel endLabel
  result <- freshReg
  emit $ "  " ++ result ++ " = phi %Value [ " ++ strResult ++ ", %" ++ strLabel ++ " ], [ " ++ listResult ++ ", %" ++ listLabel ++ " ]"
  return result

compileAt :: Expr -> Expr -> Compiler String
compileAt listExpr idxExpr = do
  val <- compileExpr listExpr
  idxVal <- compileExpr idxExpr
  
  -- Store values in memory so they're accessible across branches
  valPtr <- freshReg
  idxPtr <- freshReg
  emit $ "  " ++ valPtr ++ " = alloca %Value"
  emit $ "  " ++ idxPtr ++ " = alloca %Value"
  emit $ "  store %Value " ++ val ++ ", %Value* " ++ valPtr
  emit $ "  store %Value " ++ idxVal ++ ", %Value* " ++ idxPtr
  
  tag <- getTag val
  
  -- Check if it's a string or list
  isString <- freshReg
  emit $ "  " ++ isString ++ " = icmp eq i64 " ++ tag ++ ", 2"
  
  strLabel <- freshLabel "at.string"
  listLabel <- freshLabel "at.list"
  endLabel <- freshLabel "at.end"
  
  emit $ "  br i1 " ++ isString ++ ", label %" ++ strLabel ++ ", label %" ++ listLabel
  
  -- String indexing
  emitLabel strLabel
  valStr <- freshReg
  idxValStr <- freshReg
  emit $ "  " ++ valStr ++ " = load %Value, %Value* " ++ valPtr
  emit $ "  " ++ idxValStr ++ " = load %Value, %Value* " ++ idxPtr
  rawStr <- unboxValue valStr
  idxStr <- unboxValue idxValStr
  strPtr <- freshReg
  emit $ "  " ++ strPtr ++ " = inttoptr i64 " ++ rawStr ++ " to i8*"
  charPtr <- freshReg
  emit $ "  " ++ charPtr ++ " = getelementptr i8, i8* " ++ strPtr ++ ", i64 " ++ idxStr
  charVal <- freshReg
  emit $ "  " ++ charVal ++ " = load i8, i8* " ++ charPtr
  
  -- Create a single-character string
  newStr <- freshReg
  emit $ "  " ++ newStr ++ " = call i8* @malloc(i64 2)"
  emit $ "  store i8 " ++ charVal ++ ", i8* " ++ newStr
  nullPtr <- freshReg
  emit $ "  " ++ nullPtr ++ " = getelementptr i8, i8* " ++ newStr ++ ", i64 1"
  emit $ "  store i8 0, i8* " ++ nullPtr
  
  strResult <- boxString newStr
  emit $ "  br label %" ++ endLabel
  
  -- List indexing
  emitLabel listLabel
  valList <- freshReg
  idxValList <- freshReg
  emit $ "  " ++ valList ++ " = load %Value, %Value* " ++ valPtr
  emit $ "  " ++ idxValList ++ " = load %Value, %Value* " ++ idxPtr
  rawList <- unboxValue valList
  idxList <- unboxValue idxValList
  arrayPtr <- freshReg
  emit $ "  " ++ arrayPtr ++ " = inttoptr i64 " ++ rawList ++ " to %Array*"
  dataPtr <- freshReg
  emit $ "  " ++ dataPtr ++ " = getelementptr %Array, %Array* " ++ arrayPtr ++ ", i32 0, i32 1"
  elemsPtr <- freshReg
  emit $ "  " ++ elemsPtr ++ " = load %Value*, %Value** " ++ dataPtr
  elemPtr <- freshReg
  emit $ "  " ++ elemPtr ++ " = getelementptr %Value, %Value* " ++ elemsPtr ++ ", i64 " ++ idxList
  listResult <- freshReg
  emit $ "  " ++ listResult ++ " = load %Value, %Value* " ++ elemPtr
  emit $ "  br label %" ++ endLabel
  
  emitLabel endLabel
  result <- freshReg
  emit $ "  " ++ result ++ " = phi %Value [ " ++ strResult ++ ", %" ++ strLabel ++ " ], [ " ++ listResult ++ ", %" ++ listLabel ++ " ]"
  return result

compileReverse :: Expr -> Compiler String
compileReverse arg = do
  val <- compileExpr arg
  tag <- getTag val
  raw <- unboxValue val
  
  isString <- freshReg
  emit $ "  " ++ isString ++ " = icmp eq i64 " ++ tag ++ ", 2"
  
  strLabel <- freshLabel "reverse.string"
  listLabel <- freshLabel "reverse.list"
  endLabel <- freshLabel "reverse.end"
  
  emit $ "  br i1 " ++ isString ++ ", label %" ++ strLabel ++ ", label %" ++ listLabel
  
  -- String reverse
  emitLabel strLabel
  strPtr <- freshReg
  emit $ "  " ++ strPtr ++ " = inttoptr i64 " ++ raw ++ " to i8*"
  
  lenReg <- freshReg
  emit $ "  " ++ lenReg ++ " = call i64 @strlen(i8* " ++ strPtr ++ ")"
  
  sizeReg <- freshReg
  emit $ "  " ++ sizeReg ++ " = add i64 " ++ lenReg ++ ", 1"
  newStr <- freshReg
  emit $ "  " ++ newStr ++ " = call i8* @malloc(i64 " ++ sizeReg ++ ")"
  
  -- Copy string in reverse with loop
  loopLabel <- freshLabel "reverse.str.loop"
  bodyLabel <- freshLabel "reverse.str.body"
  doneLabel <- freshLabel "reverse.str.done"
  
  counterPtr <- freshReg
  emit $ "  " ++ counterPtr ++ " = alloca i64"
  emit $ "  store i64 0, i64* " ++ counterPtr
  emit $ "  br label %" ++ loopLabel
  
  emitLabel loopLabel
  counter <- freshReg
  emit $ "  " ++ counter ++ " = load i64, i64* " ++ counterPtr
  cond <- freshReg
  emit $ "  " ++ cond ++ " = icmp slt i64 " ++ counter ++ ", " ++ lenReg
  emit $ "  br i1 " ++ cond ++ ", label %" ++ bodyLabel ++ ", label %" ++ doneLabel
  
  emitLabel bodyLabel
  -- src[i] -> dst[len-1-i]
  srcIdx <- freshReg
  emit $ "  " ++ srcIdx ++ " = sub i64 " ++ lenReg ++ ", 1"
  srcIdx2 <- freshReg
  emit $ "  " ++ srcIdx2 ++ " = sub i64 " ++ srcIdx ++ ", " ++ counter
  
  srcCharPtr <- freshReg
  emit $ "  " ++ srcCharPtr ++ " = getelementptr i8, i8* " ++ strPtr ++ ", i64 " ++ srcIdx2
  srcChar <- freshReg
  emit $ "  " ++ srcChar ++ " = load i8, i8* " ++ srcCharPtr
  
  dstCharPtr <- freshReg
  emit $ "  " ++ dstCharPtr ++ " = getelementptr i8, i8* " ++ newStr ++ ", i64 " ++ counter
  emit $ "  store i8 " ++ srcChar ++ ", i8* " ++ dstCharPtr
  
  nextCounter <- freshReg
  emit $ "  " ++ nextCounter ++ " = add i64 " ++ counter ++ ", 1"
  emit $ "  store i64 " ++ nextCounter ++ ", i64* " ++ counterPtr
  emit $ "  br label %" ++ loopLabel
  
  emitLabel doneLabel
  nullPtr <- freshReg
  emit $ "  " ++ nullPtr ++ " = getelementptr i8, i8* " ++ newStr ++ ", i64 " ++ lenReg
  emit $ "  store i8 0, i8* " ++ nullPtr
  
  boxedStr <- boxString newStr
  emit $ "  br label %" ++ endLabel
  
  -- List reverse
  emitLabel listLabel
  arrayPtr <- freshReg
  emit $ "  " ++ arrayPtr ++ " = inttoptr i64 " ++ raw ++ " to %Array*"
  
  sizePtr <- freshReg
  emit $ "  " ++ sizePtr ++ " = getelementptr %Array, %Array* " ++ arrayPtr ++ ", i32 0, i32 0"
  size <- freshReg
  emit $ "  " ++ size ++ " = load i64, i64* " ++ sizePtr
  
  newArrayPtr <- freshReg
  emit $ "  " ++ newArrayPtr ++ " = call i8* @malloc(i64 16)"
  newArrayTyped <- freshReg
  emit $ "  " ++ newArrayTyped ++ " = bitcast i8* " ++ newArrayPtr ++ " to %Array*"
  
  allocSize <- freshReg
  emit $ "  " ++ allocSize ++ " = mul i64 " ++ size ++ ", 16"
  newElemsPtr <- freshReg
  emit $ "  " ++ newElemsPtr ++ " = call i8* @malloc(i64 " ++ allocSize ++ ")"
  newElemsTyped <- freshReg
  emit $ "  " ++ newElemsTyped ++ " = bitcast i8* " ++ newElemsPtr ++ " to %Value*"
  
  dataPtr <- freshReg
  emit $ "  " ++ dataPtr ++ " = getelementptr %Array, %Array* " ++ arrayPtr ++ ", i32 0, i32 1"
  oldElemsPtr <- freshReg
  emit $ "  " ++ oldElemsPtr ++ " = load %Value*, %Value** " ++ dataPtr
  
  -- Copy elements in reverse with loop
  listLoopLabel <- freshLabel "reverse.list.loop"
  listBodyLabel <- freshLabel "reverse.list.body"
  listDoneLabel <- freshLabel "reverse.list.done"
  
  listCounterPtr <- freshReg
  emit $ "  " ++ listCounterPtr ++ " = alloca i64"
  emit $ "  store i64 0, i64* " ++ listCounterPtr
  emit $ "  br label %" ++ listLoopLabel
  
  emitLabel listLoopLabel
  listCounter <- freshReg
  emit $ "  " ++ listCounter ++ " = load i64, i64* " ++ listCounterPtr
  listCond <- freshReg
  emit $ "  " ++ listCond ++ " = icmp slt i64 " ++ listCounter ++ ", " ++ size
  emit $ "  br i1 " ++ listCond ++ ", label %" ++ listBodyLabel ++ ", label %" ++ listDoneLabel
  
  emitLabel listBodyLabel
  -- src[size-1-i] -> dst[i]
  srcListIdx <- freshReg
  emit $ "  " ++ srcListIdx ++ " = sub i64 " ++ size ++ ", 1"
  srcListIdx2 <- freshReg
  emit $ "  " ++ srcListIdx2 ++ " = sub i64 " ++ srcListIdx ++ ", " ++ listCounter
  
  srcElemPtr <- freshReg
  emit $ "  " ++ srcElemPtr ++ " = getelementptr %Value, %Value* " ++ oldElemsPtr ++ ", i64 " ++ srcListIdx2
  srcElem <- freshReg
  emit $ "  " ++ srcElem ++ " = load %Value, %Value* " ++ srcElemPtr
  
  dstElemPtr <- freshReg
  emit $ "  " ++ dstElemPtr ++ " = getelementptr %Value, %Value* " ++ newElemsTyped ++ ", i64 " ++ listCounter
  emit $ "  store %Value " ++ srcElem ++ ", %Value* " ++ dstElemPtr
  
  listNextCounter <- freshReg
  emit $ "  " ++ listNextCounter ++ " = add i64 " ++ listCounter ++ ", 1"
  emit $ "  store i64 " ++ listNextCounter ++ ", i64* " ++ listCounterPtr
  emit $ "  br label %" ++ listLoopLabel
  
  emitLabel listDoneLabel
  newSizePtr <- freshReg
  emit $ "  " ++ newSizePtr ++ " = getelementptr %Array, %Array* " ++ newArrayTyped ++ ", i32 0, i32 0"
  emit $ "  store i64 " ++ size ++ ", i64* " ++ newSizePtr
  
  newDataPtr <- freshReg
  emit $ "  " ++ newDataPtr ++ " = getelementptr %Array, %Array* " ++ newArrayTyped ++ ", i32 0, i32 1"
  emit $ "  store %Value* " ++ newElemsTyped ++ ", %Value** " ++ newDataPtr
  
  ptrInt <- freshReg
  emit $ "  " ++ ptrInt ++ " = ptrtoint i8* " ++ newArrayPtr ++ " to i64"
  boxedList <- freshReg
  emit $ "  " ++ boxedList ++ " = insertvalue %Value { i64 3, i64 undef }, i64 " ++ ptrInt ++ ", 1"
  emit $ "  br label %" ++ endLabel
  
  emitLabel endLabel
  result <- freshReg
  emit $ "  " ++ result ++ " = phi %Value [ " ++ boxedStr ++ ", %" ++ doneLabel ++ " ], [ " ++ boxedList ++ ", %" ++ listDoneLabel ++ " ]"
  return result

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
  let builtins = ["print", "len", "concat", "substring", "charAt",
                  "toUpper", "toLower", "abs", "min", "max", "pow",
                  "isInt", "isBool", "isString", "isList",
                  "readFile", "writeFile", "appendFile",
                  "head", "tail", "at", "reverse"]
      bodyFree = freeVars body
      capturedNames = S.toList $ bodyFree S.\\ S.fromList params
                                          S.\\ S.fromList funcs
                                          S.\\ S.fromList builtins
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
