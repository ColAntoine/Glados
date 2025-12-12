module TestHelper where

import Parser.LISP (parseProgram)
import Evaluator (initialEnv, evalProgram, showValue)
import Data.List (intercalate)

-- | Evaluate a Lisp program string and return the result as a string
-- This mirrors the behavior of the CLI but works directly in tests
evalLisp :: String -> Either String String
evalLisp input = do
  exprs <- parseProgram input
  let (vals, merr) = evalProgram initialEnv exprs
  case merr of
    Just err -> Left err
    Nothing -> Right (intercalate "\n" (map showValue vals) ++ "\n")
