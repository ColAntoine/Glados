module Main where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import Text.Megaparsec.Error (errorBundlePretty)
import qualified Parser as P
import Interpreter
import Compiler (compileProgramToFile)
import System.IO (hPutStrLn, stderr)
import System.Process (callProcess, readProcessWithExitCode)
import System.Directory (removeFile)
import System.FilePath (dropExtension, takeBaseName)

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Just (Interpret file) -> runInterpreter file
        Just (Compile file output) -> runCompiler file output
        Nothing -> do
            hPutStrLn stderr "Usage: glados -i <file>           (interpret)"
            hPutStrLn stderr "       glados -c <file> [-o out]  (compile)"
            exitWith (ExitFailure 84)

data Mode = Interpret FilePath | Compile FilePath FilePath

parseArgs :: [String] -> Maybe Mode
parseArgs ["-i", file] = Just (Interpret file)
parseArgs ["-c", file] = Just (Compile file (dropExtension file))
parseArgs ["-c", file, "-o", out] = Just (Compile file out)
parseArgs _ = Nothing

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

runCompiler :: FilePath -> FilePath -> IO ()
runCompiler file output = do
    input <- readFile file
    case P.parseProgram input of
        Left err -> do
            hPutStrLn stderr (errorBundlePretty err)
            exitWith (ExitFailure 84)
        Right prog -> do
            let llFile = output ++ ".ll"
            compileProgramToFile prog llFile
            -- Call clang to compile the LLVM IR
            (exitCode, _, clangErr) <- readProcessWithExitCode "clang" [llFile, "-o", output] ""
            case exitCode of
                ExitSuccess -> do
                    removeFile llFile
                    return ()
                ExitFailure _ -> do
                    hPutStrLn stderr $ "Clang compilation failed:\n" ++ clangErr
                    exitWith (ExitFailure 84)
