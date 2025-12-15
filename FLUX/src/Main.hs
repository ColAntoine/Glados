module Main where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import Text.Megaparsec.Error (errorBundlePretty)
import qualified Parser as P
import AST
import Interpreter
import System.IO (hPutStrLn, stderr)
import Control.Monad (when)

main :: IO ()
main = do
    args <- getArgs
    let (showAst, fileArgs) = if "--AST" `elem` args 
                               then (True, filter (/= "--AST") args)
                               else (False, args)
    input <- case fileArgs of
        (f:_) -> readFile f
        _ -> getContents
    case P.parseProgram input of
        Left err -> putStrLn (errorBundlePretty err) >> exitWith (ExitFailure 84)
        Right prog -> do
            when showAst $ do
                putStrLn "=== PARSED AST ==="
                print prog
                putStrLn "\n=== DESUGARED AST ==="
                print (map desugarTopLevel prog)
                putStrLn "\n=== EXECUTION ==="
            r <- runProgram prog
            case r of
              Left err -> hPutStrLn stderr ("*** ERROR : " ++ err ++ (if not (null err) && last err == '.' then "" else ".")) >> exitWith (ExitFailure 84)
              Right () -> pure ()

desugarTopLevel :: TopLevel -> TopLevel
desugarTopLevel (TLFn name params body) = TLFn name params (P.desugarPipes body)
desugarTopLevel (TLProc name params stmts) = TLProc name params (map desugarTopLevel stmts)
desugarTopLevel (TLLet name expr) = TLLet name (P.desugarPipes expr)
desugarTopLevel (TLExpr expr) = TLExpr (P.desugarPipes expr)
