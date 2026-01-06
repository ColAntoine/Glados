module Main where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import Text.Megaparsec.Error (errorBundlePretty)
import qualified Parser as P
import Interpreter
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-i", file] -> runInterpreter file
        _ -> do
            hPutStrLn stderr "Usage: glados -i <file>"
            exitWith (ExitFailure 84)

runInterpreter :: FilePath -> IO ()
runInterpreter file = do
    input <- readFile file
    case P.parseProgram input of
        Left err -> do
            hPutStrLn stderr (errorBundlePretty err)
            exitWith (ExitFailure 84)
        Right prog -> do
            r <- runProgram prog
            case r of
                Left err -> do
                    hPutStrLn stderr ("*** ERROR : " ++ err ++ if not (null err) && last err == '.' then "" else ".")
                    exitWith (ExitFailure 84)
                Right mval -> case mval of
                    Just (VInt n) -> exitWith (if n == 0 then ExitSuccess else ExitFailure (fromIntegral n))
                    _ -> exitWith ExitSuccess
