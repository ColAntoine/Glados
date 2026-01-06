{-# LANGUAGE OverloadedStrings #-}
module Compiler
  ( compileProgram
  , compileProgramToFile
  ) where

import AST
import qualified Parser as P
import Data.Int (Int64)
import Data.List (intercalate)
import Control.Monad.State
import Control.Monad (forM, forM_, foldM)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

-- | Value tags for boxed runtime values
-- Tag 0 = Int64
-- Tag 1 = Bool  
-- Tag 2 = String (pointer)
-- Tag 3 = List (pointer to array)
-- Tag 4 = Tuple (pointer to array)
-- Tag 5 = Closure (pointer to closure struct)

data CompilerState = CompilerState
  { csNextReg    :: Int           -- Next SSA register
  , csNextLabel  :: Int           -- Next label number
  , csNextStr    :: Int           -- Next string constant number
  , csStrings    :: [(Int, String)] -- String constants
  , csCode       :: [String]      -- Generated code lines
  , csFunctions  :: [String]      -- Generated function definitions
  , csLocals     :: M.Map String String  -- Variable name -> register
  , csFuncNames  :: [String]      -- Declared function names
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
setLocal name reg = modify $ \s -> s { csLocals = M.insert name reg (csLocals s) }

withLocals :: M.Map String String -> Compiler a -> Compiler a
withLocals newLocals action = do
  oldLocals <- gets csLocals
  modify $ \s -> s { csLocals = M.union newLocals oldLocals }
  result <- action
  modify $ \s -> s { csLocals = oldLocals }
  return result

-- | Runtime type structure: { i64 tag, i64 value }
-- For pointers, value holds the pointer as i64

-- | Generate LLVM IR prelude with runtime support
genPrelude :: Compiler ()
genPrelude = do
  -- Declare external functions
  emitFunc "; Runtime declarations"
  emitFunc "declare i32 @printf(i8*, ...)"
  emitFunc "declare i8* @malloc(i64)"
  emitFunc "declare void @free(i8*)"
  emitFunc "declare i8* @strcpy(i8*, i8*)"
  emitFunc "declare i64 @strlen(i8*)"
  emitFunc ""
  
  -- Value type (boxed): { tag: i64, data: i64 }
  emitFunc "; Boxed value type: { i64 tag, i64 data }"
  emitFunc "%Value = type { i64, i64 }"
  emitFunc ""
  
  -- Closure type: { funcptr, env_size, env... }
  emitFunc "; Closure type: { i8* funcptr, i64 env_size, %Value* env }"
  emitFunc "%Closure = type { i8*, i64, %Value* }"
  emitFunc ""
  
  -- List/Tuple type: { size, elements... }
  emitFunc "; Array type for lists/tuples: { i64 size, %Value* elements }"
  emitFunc "%Array = type { i64, %Value* }"
  emitFunc ""

-- | Generate string constants
genStrings :: [(Int, String)] -> [String]
genStrings strs = map genStr strs
  where
    genStr (n, s) = 
      let escaped = escapeString s
          len = length s + 1
      in "@.str." ++ show n ++ " = private unnamed_addr constant [" ++ 
         show len ++ " x i8] c\"" ++ escaped ++ "\\00\""

escapeString :: String -> String
escapeString = concatMap escapeChar
  where
    escapeChar '\n' = "\\0A"
    escapeChar '\t' = "\\09"
    escapeChar '\r' = "\\0D"
    escapeChar '\\' = "\\\\"
    escapeChar '"'  = "\\22"
    escapeChar c    = [c]

-- | Box an i64 as an Int value (tag 0)
boxInt :: String -> Compiler String
boxInt val = do
  r1 <- freshReg
  r2 <- freshReg
  result <- freshReg
  emit $ "  " ++ r1 ++ " = insertvalue %Value { i64 0, i64 undef }, i64 " ++ val ++ ", 1"
  return r1

-- | Box a boolean as a Bool value (tag 1)
boxBool :: String -> Compiler String
boxBool val = do
  r1 <- freshReg
  r2 <- freshReg
  extended <- freshReg
  emit $ "  " ++ extended ++ " = zext i1 " ++ val ++ " to i64"
  emit $ "  " ++ r1 ++ " = insertvalue %Value { i64 1, i64 undef }, i64 " ++ extended ++ ", 1"
  return r1

-- | Box a string pointer as a String value (tag 2)
boxString :: String -> Compiler String
boxString ptr = do
  r1 <- freshReg
  ptrInt <- freshReg
  emit $ "  " ++ ptrInt ++ " = ptrtoint i8* " ++ ptr ++ " to i64"
  emit $ "  " ++ r1 ++ " = insertvalue %Value { i64 2, i64 undef }, i64 " ++ ptrInt ++ ", 1"
  return r1

-- | Unbox to get raw i64 value
unboxValue :: String -> Compiler String
unboxValue boxed = do
  result <- freshReg
  emit $ "  " ++ result ++ " = extractvalue %Value " ++ boxed ++ ", 1"
  return result

-- | Get tag from boxed value
getTag :: String -> Compiler String
getTag boxed = do
  result <- freshReg
  emit $ "  " ++ result ++ " = extractvalue %Value " ++ boxed ++ ", 0"
  return result

-- | Compile an expression, returning the register holding the boxed result
compileExpr :: Expr -> Compiler String
compileExpr (EInt n) = boxInt (show n)

compileExpr (EBool b) = do
  let val = if b then "1" else "0"
  r <- freshReg
  emit $ "  " ++ r ++ " = insertvalue %Value { i64 1, i64 undef }, i64 " ++ val ++ ", 1"
  return r

compileExpr (EString s) = do
  strId <- addString s
  r1 <- freshReg
  r2 <- freshReg
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
  -- Desugar pipe: a |> b becomes b(a)
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
      -- Compare both tag and value
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
  
  emit $ "  br i1 " ++ condBool ++ ", label %" ++ thenLabel ++ ", label %" ++ elseLabel
  
  -- Then branch
  emit $ thenLabel ++ ":"
  thenResult <- compileExpr thenE
  thenEndLabel <- freshLabel "then.end"
  emit $ "  br label %" ++ endLabel
  let thenBlock = thenLabel
  
  -- Else branch  
  emit $ elseLabel ++ ":"
  elseResult <- compileExpr elseE
  emit $ "  br label %" ++ endLabel
  let elseBlock = elseLabel
  
  -- Merge
  emit $ endLabel ++ ":"
  result <- freshReg
  emit $ "  " ++ result ++ " = phi %Value [ " ++ thenResult ++ ", %" ++ thenBlock ++ 
         " ], [ " ++ elseResult ++ ", %" ++ elseBlock ++ " ]"
  return result

compileExpr (ECall (EVar "print") [arg]) = do
  val <- compileExpr arg
  tag <- getTag val
  raw <- unboxValue val
  
  -- Branch based on tag
  isInt <- freshReg
  isBool <- freshReg
  isStr <- freshReg
  
  emit $ "  " ++ isInt ++ " = icmp eq i64 " ++ tag ++ ", 0"
  
  intLabel <- freshLabel "print.int"
  boolLabel <- freshLabel "print.bool"
  strLabel <- freshLabel "print.str"
  endLabel <- freshLabel "print.end"
  notIntLabel <- freshLabel "print.notint"
  
  emit $ "  br i1 " ++ isInt ++ ", label %" ++ intLabel ++ ", label %" ++ notIntLabel
  
  -- Print int
  emit $ intLabel ++ ":"
  intFmt <- addString "%ld\n"
  fmtPtr1 <- freshReg
  emit $ "  " ++ fmtPtr1 ++ " = getelementptr [6 x i8], [6 x i8]* @.str." ++ show intFmt ++ ", i64 0, i64 0"
  emit $ "  call i32 (i8*, ...) @printf(i8* " ++ fmtPtr1 ++ ", i64 " ++ raw ++ ")"
  emit $ "  br label %" ++ endLabel
  
  -- Check if bool
  emit $ notIntLabel ++ ":"
  emit $ "  " ++ isBool ++ " = icmp eq i64 " ++ tag ++ ", 1"
  emit $ "  br i1 " ++ isBool ++ ", label %" ++ boolLabel ++ ", label %" ++ strLabel
  
  -- Print bool
  emit $ boolLabel ++ ":"
  trueStr <- addString "#t\n"
  falseStr <- addString "#f\n"
  boolCond <- freshReg
  emit $ "  " ++ boolCond ++ " = icmp ne i64 " ++ raw ++ ", 0"
  truePtr <- freshReg
  falsePtr <- freshReg
  emit $ "  " ++ truePtr ++ " = getelementptr [4 x i8], [4 x i8]* @.str." ++ show trueStr ++ ", i64 0, i64 0"
  emit $ "  " ++ falsePtr ++ " = getelementptr [4 x i8], [4 x i8]* @.str." ++ show falseStr ++ ", i64 0, i64 0"
  selPtr <- freshReg
  emit $ "  " ++ selPtr ++ " = select i1 " ++ boolCond ++ ", i8* " ++ truePtr ++ ", i8* " ++ falsePtr
  emit $ "  call i32 (i8*, ...) @printf(i8* " ++ selPtr ++ ")"
  emit $ "  br label %" ++ endLabel
  
  -- Print string
  emit $ strLabel ++ ":"
  strPtr <- freshReg
  strFmt <- addString "%s\n"
  emit $ "  " ++ strPtr ++ " = inttoptr i64 " ++ raw ++ " to i8*"
  fmtPtr2 <- freshReg
  emit $ "  " ++ fmtPtr2 ++ " = getelementptr [4 x i8], [4 x i8]* @.str." ++ show strFmt ++ ", i64 0, i64 0"
  emit $ "  call i32 (i8*, ...) @printf(i8* " ++ fmtPtr2 ++ ", i8* " ++ strPtr ++ ")"
  emit $ "  br label %" ++ endLabel
  
  emit $ endLabel ++ ":"
  return val

compileExpr (ECall (EVar name) args) = do
  -- Check if it's a known function
  funcs <- gets csFuncNames
  if name `elem` funcs
    then do
      -- Compile arguments and allocate space for pointers
      argPtrs <- forM args $ \arg -> do
        val <- compileExpr arg
        ptr <- freshReg
        emit $ "  " ++ ptr ++ " = alloca %Value"
        emit $ "  store %Value " ++ val ++ ", %Value* " ++ ptr
        return ptr
      result <- freshReg
      let argList = intercalate ", " (map ("%Value* " ++) argPtrs)
      emit $ "  " ++ result ++ " = call %Value @" ++ name ++ "(" ++ argList ++ ")"
      return result
    else do
      -- Try to load as closure from local
      mReg <- getLocal name
      case mReg of
        Just reg -> do
          -- Load closure and call it
          closureVal <- freshReg
          emit $ "  " ++ closureVal ++ " = load %Value, %Value* " ++ reg
          compileClosureCall closureVal args
        Nothing -> error $ "Unknown function: " ++ name

compileExpr (ECall funcExpr args) = do
  -- General function call (closure)
  closureVal <- compileExpr funcExpr
  compileClosureCall closureVal args

compileExpr (ELam params body) = do
  -- Create a closure
  -- For simplicity, we'll create named functions and capture environment
  closureName <- freshLabel "lambda"
  
  -- Generate the lambda function
  let paramList = intercalate ", " ["%Value* %" ++ p ++ ".ptr" | p <- params]
  emitFunc $ "define %Value @" ++ closureName ++ "(" ++ paramList ++ ") {"
  emitFunc "entry:"
  
  -- Save current state and compile body in new context
  oldCode <- gets csCode
  oldLocals <- gets csLocals
  modify $ \s -> s { csCode = [], csLocals = M.empty }
  
  -- Set up parameter locals
  forM_ params $ \p -> do
    setLocal p ("%" ++ p ++ ".ptr")
  
  bodyResult <- compileExpr body
  emit $ "  ret %Value " ++ bodyResult
  
  bodyCode <- gets csCode
  mapM_ emitFunc bodyCode
  emitFunc "}"
  emitFunc ""
  
  -- Restore state
  modify $ \s -> s { csCode = oldCode, csLocals = oldLocals }
  
  -- Return a closure value (simplified: just function pointer as i64)
  funcPtr <- freshReg
  result <- freshReg
  emit $ "  " ++ funcPtr ++ " = ptrtoint %Value (" ++ 
         intercalate ", " (replicate (length params) "%Value*") ++ 
         ")* @" ++ closureName ++ " to i64"
  emit $ "  " ++ result ++ " = insertvalue %Value { i64 5, i64 undef }, i64 " ++ funcPtr ++ ", 1"
  return result

compileExpr (EList elems) = do
  -- Allocate array structure
  let n = length elems
  arrayPtr <- freshReg
  elemsPtr <- freshReg
  
  emit $ "  " ++ arrayPtr ++ " = call i8* @malloc(i64 16)"  -- sizeof(%Array)
  arrayTyped <- freshReg
  emit $ "  " ++ arrayTyped ++ " = bitcast i8* " ++ arrayPtr ++ " to %Array*"
  
  -- Allocate elements
  if n > 0 then do
    emit $ "  " ++ elemsPtr ++ " = call i8* @malloc(i64 " ++ show (n * 16) ++ ")"
    elemsTyped <- freshReg
    emit $ "  " ++ elemsTyped ++ " = bitcast i8* " ++ elemsPtr ++ " to %Value*"
    
    -- Store size
    sizePtr <- freshReg
    emit $ "  " ++ sizePtr ++ " = getelementptr %Array, %Array* " ++ arrayTyped ++ ", i32 0, i32 0"
    emit $ "  store i64 " ++ show n ++ ", i64* " ++ sizePtr
    
    -- Store elements pointer
    dataPtr <- freshReg
    emit $ "  " ++ dataPtr ++ " = getelementptr %Array, %Array* " ++ arrayTyped ++ ", i32 0, i32 1"
    emit $ "  store %Value* " ++ elemsTyped ++ ", %Value** " ++ dataPtr
    
    -- Store each element
    forM_ (zip [0..] elems) $ \(i, elem) -> do
      val <- compileExpr elem
      elemPtr <- freshReg
      emit $ "  " ++ elemPtr ++ " = getelementptr %Value, %Value* " ++ elemsTyped ++ ", i64 " ++ show i
      emit $ "  store %Value " ++ val ++ ", %Value* " ++ elemPtr
  else do
    -- Empty list
    sizePtr <- freshReg
    emit $ "  " ++ sizePtr ++ " = getelementptr %Array, %Array* " ++ arrayTyped ++ ", i32 0, i32 0"
    emit $ "  store i64 0, i64* " ++ sizePtr
    dataPtr <- freshReg
    emit $ "  " ++ dataPtr ++ " = getelementptr %Array, %Array* " ++ arrayTyped ++ ", i32 0, i32 1"
    emit $ "  store %Value* null, %Value** " ++ dataPtr
  
  -- Box the array pointer as list (tag 3)
  ptrInt <- freshReg
  result <- freshReg
  emit $ "  " ++ ptrInt ++ " = ptrtoint i8* " ++ arrayPtr ++ " to i64"
  emit $ "  " ++ result ++ " = insertvalue %Value { i64 3, i64 undef }, i64 " ++ ptrInt ++ ", 1"
  return result

compileExpr (ETuple elems) = do
  -- Same as list but with tag 4
  let n = length elems
  arrayPtr <- freshReg
  
  emit $ "  " ++ arrayPtr ++ " = call i8* @malloc(i64 16)"
  arrayTyped <- freshReg
  emit $ "  " ++ arrayTyped ++ " = bitcast i8* " ++ arrayPtr ++ " to %Array*"
  
  elemsPtr <- freshReg
  emit $ "  " ++ elemsPtr ++ " = call i8* @malloc(i64 " ++ show (n * 16) ++ ")"
  elemsTyped <- freshReg
  emit $ "  " ++ elemsTyped ++ " = bitcast i8* " ++ elemsPtr ++ " to %Value*"
  
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
  emit $ "  " ++ result ++ " = insertvalue %Value { i64 4, i64 undef }, i64 " ++ ptrInt ++ ", 1"
  return result

compileExpr (EBlock stmts mExpr) = do
  -- Compile statements
  forM_ stmts compileTopLevel
  -- Return final expression or unit
  case mExpr of
    Just e -> compileExpr e
    Nothing -> boxInt "0"

compileExpr (ERet e) = compileExpr e

compileClosureCall :: String -> [Expr] -> Compiler String
compileClosureCall closureVal args = do
  -- For now, simplified: assume direct function call
  -- In a full implementation, we'd extract the function pointer from closure
  funcPtr <- unboxValue closureVal
  
  -- Compile arguments and allocate space for them
  argRegs <- forM args $ \arg -> do
    val <- compileExpr arg
    ptr <- freshReg
    emit $ "  " ++ ptr ++ " = alloca %Value"
    emit $ "  store %Value " ++ val ++ ", %Value* " ++ ptr
    return ptr
  
  -- Cast function pointer and call
  let nArgs = length args
  funcTyped <- freshReg
  let argTypes = intercalate ", " (replicate nArgs "%Value*")
  emit $ "  " ++ funcTyped ++ " = inttoptr i64 " ++ funcPtr ++ " to %Value (" ++ argTypes ++ ")*"
  
  result <- freshReg
  let argList = intercalate ", " ["%Value* " ++ r | r <- argRegs]
  emit $ "  " ++ result ++ " = call %Value " ++ funcTyped ++ "(" ++ argList ++ ")"
  return result

-- | Compile a top-level form
compileTopLevel :: TopLevel -> Compiler ()
compileTopLevel (TLImport _ _) = return ()  -- Imports are handled at the top level
compileTopLevel (TLFn name params body) = do
  -- Add to known functions
  modify $ \s -> s { csFuncNames = name : csFuncNames s }
  
  -- Generate function
  let paramList = intercalate ", " ["%Value* %" ++ p ++ ".ptr" | p <- params]
  emitFunc $ "define %Value @" ++ name ++ "(" ++ paramList ++ ") {"
  emitFunc "entry:"
  
  -- Save state
  oldCode <- gets csCode
  oldLocals <- gets csLocals
  modify $ \s -> s { csCode = [], csLocals = M.empty }
  
  -- Set up parameters
  forM_ params $ \p -> setLocal p ("%" ++ p ++ ".ptr")
  
  -- Compile body
  let body' = P.desugarPipes body
  result <- compileExpr body'
  emit $ "  ret %Value " ++ result
  
  bodyCode <- gets csCode
  mapM_ emitFunc bodyCode
  emitFunc "}"
  emitFunc ""
  
  -- Restore state
  modify $ \s -> s { csCode = oldCode, csLocals = oldLocals }

compileTopLevel (TLProc name params stmts) = do
  -- Similar to TLFn but with block body
  modify $ \s -> s { csFuncNames = name : csFuncNames s }
  
  let paramList = intercalate ", " ["%Value* %" ++ p ++ ".ptr" | p <- params]
  emitFunc $ "define %Value @" ++ name ++ "(" ++ paramList ++ ") {"
  emitFunc "entry:"
  
  oldCode <- gets csCode
  oldLocals <- gets csLocals
  modify $ \s -> s { csCode = [], csLocals = M.empty }
  
  forM_ params $ \p -> setLocal p ("%" ++ p ++ ".ptr")
  
  -- Compile statements
  forM_ stmts compileTopLevel
  
  -- Return 0
  result <- boxInt "0"
  emit $ "  ret %Value " ++ result
  
  bodyCode <- gets csCode
  mapM_ emitFunc bodyCode
  emitFunc "}"
  emitFunc ""
  
  modify $ \s -> s { csCode = oldCode, csLocals = oldLocals }

compileTopLevel (TLLet name expr) = do
  let expr' = P.desugarPipes expr
  val <- compileExpr expr'
  ptr <- freshReg
  emit $ "  " ++ ptr ++ " = alloca %Value"
  emit $ "  store %Value " ++ val ++ ", %Value* " ++ ptr
  setLocal name ptr

compileTopLevel (TLExpr expr) = do
  let expr' = P.desugarPipes expr
  _ <- compileExpr expr'
  return ()

-- | Compile a program to LLVM IR
compileProgram :: Program -> String
compileProgram prog = evalState action initialState
  where
    action = do
      genPrelude
      
      -- First pass: collect all function names
      forM_ prog $ \tl -> case tl of
        TLFn name _ _ -> modify $ \s -> s { csFuncNames = name : csFuncNames s }
        TLProc name _ _ -> modify $ \s -> s { csFuncNames = name : csFuncNames s }
        _ -> return ()
      
      -- Compile all top-level functions first
      forM_ prog $ \tl -> case tl of
        TLFn {} -> compileTopLevel tl
        TLProc {} -> compileTopLevel tl
        _ -> return ()
      
      -- Generate main function with remaining top-level code
      emit "define i32 @main() {"
      emit "entry:"
      
      lastVal <- foldM compileMain Nothing prog
      
      -- Return exit code based on last value
      case lastVal of
        Just reg -> do
          tag <- getTag reg
          raw <- unboxValue reg
          isInt <- freshReg
          emit $ "  " ++ isInt ++ " = icmp eq i64 " ++ tag ++ ", 0"
          
          retInt <- freshLabel "ret.int"
          retZero <- freshLabel "ret.zero"
          
          emit $ "  br i1 " ++ isInt ++ ", label %" ++ retInt ++ ", label %" ++ retZero
          
          emit $ retInt ++ ":"
          truncated <- freshReg
          emit $ "  " ++ truncated ++ " = trunc i64 " ++ raw ++ " to i32"
          emit $ "  ret i32 " ++ truncated
          
          emit $ retZero ++ ":"
          emit "  ret i32 0"
        Nothing -> emit "  ret i32 0"
      
      emit "}"
      
      -- Collect all parts
      funcs <- gets csFunctions
      strs <- gets csStrings
      mainCode <- gets csCode
      
      return $ unlines $
        ["; Generated by Flux compiler", ""] ++
        genStrings strs ++
        [""] ++
        funcs ++
        mainCode
    
    compileMain mLast tl = case tl of
      TLImport {} -> return mLast  -- Already handled
      TLFn {} -> return mLast  -- Already compiled
      TLProc {} -> return mLast  -- Already compiled
      TLLet name expr -> do
        compileTopLevel (TLLet name expr)
        return mLast
      TLExpr expr -> do
        let expr' = P.desugarPipes expr
        result <- compileExpr expr'
        return (Just result)

-- | Compile and write to file
compileProgramToFile :: Program -> FilePath -> IO ()
compileProgramToFile prog path = do
  let ir = compileProgram prog
  writeFile path ir
