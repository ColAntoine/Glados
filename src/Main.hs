module Main (main) where

import System.IO (hGetContents, stdin, hPutStrLn, stderr, writeFile)
import System.Exit (exitWith, ExitCode(..))
import System.Environment (getArgs)
import System.Process (system)
import System.Directory (removeFile)
import Control.Exception (try, SomeException)
import Data.List (intercalate)
import Parser.LISP (parseProgram)
import Evaluator (initialEnv, evalProgram, showValue, containsUnbound)
import Compiler (compileProgram)
import Parser.GLaDOS (parseGLaDOSProgram)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-lisp"] -> runLispMode
        []        -> runGLaDOSMode
        _         -> hPutStrLn stderr "Usage: glados [-lisp]" >> exitWith (ExitFailure 84)

runLispMode :: IO ()
runLispMode = do
    input <- hGetContents stdin
    case parseProgram input of
        Left err -> hPutStrLn stderr (formatErr err) >> exitWith (ExitFailure 84)
        Right exprs -> do
            let (vals, merr) = evalProgram initialEnv exprs
            whenNotNull vals (putStrLn (intercalate "\n" (map showValue vals)))
            case merr of
                Just err -> hPutStrLn stderr (formatErr err) >> exitWith (ExitFailure 84)
                Nothing -> if any containsUnbound vals
                              then exitWith (ExitFailure 84)
                              else pure ()

runGLaDOSMode :: IO ()
runGLaDOSMode = do
    input <- hGetContents stdin
    case parseGLaDOSProgram input of
        Left err -> hPutStrLn stderr (formatErr err) >> exitWith (ExitFailure 84)
        Right ast -> do
            case compileProgram ast of
                Left err -> hPutStrLn stderr (formatErr err) >> exitWith (ExitFailure 84)
                Right asm -> do
                    -- Print the assembly code
                    putStrLn asm
                    putStrLn "======="
                    -- Compile and execute
                    result <- compileAndExecute asm
                    case result of
                        Left err -> hPutStrLn stderr (formatErr err) >> exitWith (ExitFailure 84)
                        Right exitCode -> print exitCode

whenNotNull :: [a] -> IO () -> IO ()
whenNotNull [] _ = pure ()
whenNotNull _  f = f

formatErr :: String -> String
formatErr s = "*** ERROR : " ++ fixDot s
    where
        fixDot str = if null str then str else if last str == '.' then str else str ++ "."

compileAndExecute :: String -> IO (Either String Int)
compileAndExecute asm = do
    let asmFile = "/tmp/glados_temp.s"
    let objFile = "/tmp/glados_temp.o"
    let binFile = "/tmp/glados_temp"
    
    -- Write assembly to file
    writeFile asmFile asm
    
    -- Assemble with gas (GNU assembler)
    assembleResult <- system $ "as --64 " ++ asmFile ++ " -o " ++ objFile
    case assembleResult of
        ExitFailure _ -> return $ Left "Assembly failed"
        ExitSuccess -> do
            -- Link to create executable
            linkResult <- system $ "ld " ++ objFile ++ " -o " ++ binFile
            case linkResult of
                ExitFailure _ -> return $ Left "Linking failed"
                ExitSuccess -> do
                    -- Execute and capture exit code
                    execResult <- system binFile
                    -- Clean up temporary files
                    _ <- try (removeFile asmFile) :: IO (Either SomeException ())
                    _ <- try (removeFile objFile) :: IO (Either SomeException ())
                    _ <- try (removeFile binFile) :: IO (Either SomeException ())
                    
                    case execResult of
                        ExitFailure code -> return $ Right code
                        ExitSuccess -> return $ Right 0