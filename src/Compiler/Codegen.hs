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
  emitFunc "declare i32 @strcmp(i8*, i8*)",
  emitFunc "declare void @exit(i32)",
  emitFunc "declare void @llvm.memcpy.p0i8.p0i8.i64(i8*, i8*, i64, i1)",
  emitFunc "",
  emitFunc "; File I/O declarations",
  emitFunc "declare i8* @fopen(i8*, i8*)",
  emitFunc "declare i32 @fclose(i8*)",
  emitFunc "declare i64 @fread(i8*, i64, i64, i8*)",
  emitFunc "declare i64 @fwrite(i8*, i64, i64, i8*)",
  emitFunc "declare i32 @fseek(i8*, i64, i32)",
  emitFunc "declare i64 @ftell(i8*)",
  emitFunc "",
  emitFunc "; Boxed value type: { i64 tag, i64 data }",
  emitFunc "%Value = type { i64, i64 }",
  emitFunc "",
  emitFunc "; Closure type: { i8* funcptr, i64 env_size, %Value* env }",
  emitFunc "%Closure = type { i8*, i64, %Value* }",
  emitFunc "",
  emitFunc "; Array type for lists/tuples: { i64 size, %Value* elements }",
  emitFunc "%Array = type { i64, %Value* }",
  emitFunc "",
  emitFunc "; C ABI helper functions",
  emitFunc "define %Value @box_int(i64 %val) {",
  emitFunc "  %boxed = insertvalue %Value { i64 0, i64 undef }, i64 %val, 1",
  emitFunc "  ret %Value %boxed",
  emitFunc "}",
  emitFunc "",
  emitFunc "define i64 @unbox_int(%Value %val) {",
  emitFunc "  %raw = extractvalue %Value %val, 1",
  emitFunc "  ret i64 %raw",
  emitFunc "}",
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
