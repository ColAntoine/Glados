{-# LANGUAGE OverloadedStrings #-}
module Compiler.Codegen where

import Compiler.Types

genPrelude :: Compiler ()
genPrelude = sequence_ [
  emitFunc "; Runtime declarations",
  emitFunc "declare i32 @printf(i8*, ...)",
  emitFunc "declare i8* @malloc(i64)",
  emitFunc "declare void @free(i8*)",
  emitFunc "declare i8* @strcpy(i8*, i8*)",
  emitFunc "declare i64 @strlen(i8*)",
  emitFunc "",
  emitFunc "; Boxed value type: { i64 tag, i64 data }",
  emitFunc "%Value = type { i64, i64 }",
  emitFunc "",
  emitFunc "; Closure type: { i8* funcptr, i64 env_size, %Value* env }",
  emitFunc "%Closure = type { i8*, i64, %Value* }",
  emitFunc "",
  emitFunc "; Array type for lists/tuples: { i64 size, %Value* elements }",
  emitFunc "%Array = type { i64, %Value* }",
  emitFunc ""
  ]

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
