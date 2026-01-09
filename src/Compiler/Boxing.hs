{-# LANGUAGE OverloadedStrings #-}
module Compiler.Boxing where

import Compiler.Types

boxInt :: String -> Compiler String
boxInt val = do
  r1 <- freshReg
  emit $ "  " ++ r1 ++
    " = insertvalue %Value { i64 0, i64 undef }, i64 " ++ val ++ ", 1"
  return r1

boxBool :: String -> Compiler String
boxBool val = do
  extended <- freshReg
  r1 <- freshReg
  emit $ "  " ++ extended ++ " = zext i1 " ++ val ++ " to i64"
  emit $ "  " ++ r1 ++
    " = insertvalue %Value { i64 1, i64 undef }, i64 " ++
    extended ++ ", 1"
  return r1

boxString :: String -> Compiler String
boxString ptr = do
  r1 <- freshReg
  ptrInt <- freshReg
  emit $ "  " ++ ptrInt ++ " = ptrtoint i8* " ++ ptr ++ " to i64"
  emit $ "  " ++ r1 ++
    " = insertvalue %Value { i64 2, i64 undef }, i64 " ++
    ptrInt ++ ", 1"
  return r1

boxList :: String -> Compiler String
boxList val = do
  r1 <- freshReg
  emit $ "  " ++ r1 ++
    " = insertvalue %Value { i64 3, i64 undef }, i64 " ++ val ++ ", 1"
  return r1

unboxValue :: String -> Compiler String
unboxValue boxed = do
  result <- freshReg
  emit $ "  " ++ result ++ " = extractvalue %Value " ++ boxed ++ ", 1"
  return result

getTag :: String -> Compiler String
getTag boxed = do
  result <- freshReg
  emit $ "  " ++ result ++ " = extractvalue %Value " ++ boxed ++ ", 0"
  return result
