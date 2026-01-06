{-
-- EPITECH PROJECT, 2026
-- Glados
-- File description:
-- Main entry point for Flux language interpreter and compiler
-}

module Main where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import Text.Megaparsec.Error (errorBundlePretty)
import qualified Parser as P
import Interpreter
import Compiler (compileProgramToFile)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)
import System.Directory (removeFile, doesFileExist)
import System.FilePath (dropExtension, takeDirectory, (</>))
import AST
import qualified Data.Set as Set
import Data.List (nub)
import Control.Monad (forM, foldM)

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Just (Interpret file) -> runInterpreter file
        Just (Compile files output) -> runCompilerMulti files output
        Nothing ->
            hPutStrLn stderr
              "Usage: glados -i <file>                  (interpret)" >>
            hPutStrLn stderr
              "       glados -c <files...> [-o out]  (compile)" >>
            exitWith (ExitFailure 84)

data Mode = Interpret FilePath | Compile [FilePath] FilePath

parseArgs :: [String] -> Maybe Mode
parseArgs ["-i", file] = Just (Interpret file)
parseArgs args | "-c" `elem` args = 
    let (beforeO, rest) = break (== "-o") (dropWhile (/= "-c") args)
        files = tail beforeO  -- skip "-c"
        output = case rest of
                  ["-o", out] -> out
                  _ -> dropExtension (head files)
    in if null files then Nothing else Just (Compile files output)
parseArgs _ = Nothing

runInterpreter :: FilePath -> IO ()
runInterpreter file = do
    input <- readFile file
    case P.parseProgram input of
        Left err ->
            hPutStrLn stderr (errorBundlePretty err) >>
            exitWith (ExitFailure 84)
        Right prog -> do
            r <- runProgramWithPath prog file
            case r of
                Left err ->
                    hPutStrLn stderr ("*** ERROR : " ++ err ++
                      if not (null err) && last err == '.'
                      then "" else ".") >>
                    exitWith (ExitFailure 84)
                Right mval -> case mval of
                    Just (VInt 0) -> exitWith ExitSuccess
                    Just (VInt n) -> exitWith (ExitFailure (fromIntegral n))
                    _ -> exitWith ExitSuccess

-- | Load and parse a single file
loadFile :: FilePath -> IO (Either String Program)
loadFile file = do
    exists <- doesFileExist file
    if not exists
        then return $ Left $ "File not found: " ++ file
        else do
            input <- readFile file
            case P.parseProgram input of
                Left err -> return $ Left $ errorBundlePretty err
                Right prog -> return $ Right prog

-- | Recursively load all imported files
loadWithImports :: FilePath -> Set.Set FilePath -> IO (Either String Program)
loadWithImports file loaded
    | file `Set.member` loaded = return $ Right []  -- Already loaded
    | otherwise = do
        result <- loadFile file
        case result of
            Left err -> return $ Left err
            Right prog -> do
                let newLoaded = Set.insert file loaded
                -- Find all import statements
                let imports = [(path, funcs) | TLImport path funcs <- prog]
                -- Recursively load imported files
                importedProg <- foldM loadImport (Right []) imports
                case importedProg of
                    Left err -> return $ Left err
                    Right imported -> return $ Right (imported ++ prog)
  where
    loadImport acc (path, _funcs) =
        case acc of
            Left err -> return $ Left err
            Right prog -> do
                let baseDir = takeDirectory file
                let importPath = baseDir </> path
                result <- loadWithImports importPath loaded
                case result of
                    Left err -> return $ Left err
                    Right imported -> return $ Right (prog ++ imported)

-- | Load and merge multiple files
loadMultipleFiles :: [FilePath] -> IO (Either String Program)
loadMultipleFiles files = do
    results <- forM files (loadWithImports `flip` Set.empty)
    case sequence results of
        Left err -> return $ Left err
        Right programs -> return $ Right (concat programs)

-- | Check that all called functions are defined
checkFunctionDefinitions :: Program -> Either String ()
checkFunctionDefinitions prog =
    let definedFuncs = Set.fromList
          ([name | TLFn name _ _ <- prog] ++
           [name | TLProc name _ _ <- prog])
        calledFuncs = findCalledFuncs prog
        undefined = Set.filter (not . isBuiltin)
          (calledFuncs Set.\\ definedFuncs)
    in if Set.null undefined
        then Right ()
        else Left $
          "Undefined functions: " ++ show (Set.toList undefined)

-- | Find all function calls in a program
findCalledFuncs :: Program -> Set.Set String
findCalledFuncs prog =
  Set.fromList [name | TLExpr expr <- prog,
                       name <- findCallsInExpr expr] <>
  Set.unions [Set.fromList (findCallsInExpr body) |
              TLFn _ _ body <- prog] <>
  Set.unions [Set.fromList (findCallsInTopLevel top) |
              TLProc _ _ tops <- prog, top <- tops]

findCallsInExpr :: Expr -> [String]
findCallsInExpr (ECall (EVar name) args) =
  name : concatMap findCallsInExpr args
findCallsInExpr (ECall expr args) =
  findCallsInExpr expr ++ concatMap findCallsInExpr args
findCallsInExpr (EBinary _ e1 e2) =
  findCallsInExpr e1 ++ findCallsInExpr e2
findCallsInExpr (EUnary _ e) = findCallsInExpr e
findCallsInExpr (EIf e1 e2 e3) =
  findCallsInExpr e1 ++ findCallsInExpr e2 ++ findCallsInExpr e3
findCallsInExpr (ELam _ body) = findCallsInExpr body
findCallsInExpr (EList es) = concatMap findCallsInExpr es
findCallsInExpr (ETuple es) = concatMap findCallsInExpr es
findCallsInExpr (EBlock tops mexpr) =
  findCallsInTopLevel `concatMap` tops ++ maybe [] findCallsInExpr mexpr
findCallsInExpr _ = []

findCallsInTopLevel :: TopLevel -> [String]
findCallsInTopLevel (TLFn _ _ body) = findCallsInExpr body
findCallsInTopLevel (TLProc _ _ tops) = concatMap findCallsInTopLevel tops
findCallsInTopLevel (TLLet _ expr) = findCallsInExpr expr
findCallsInTopLevel (TLExpr expr) = findCallsInExpr expr
findCallsInTopLevel (TLImport _ _) = []

-- | Check if function is a builtin
isBuiltin :: String -> Bool
isBuiltin "print" = True
isBuiltin _ = False

-- | Compile multiple files
runCompilerMulti :: [FilePath] -> FilePath -> IO ()
runCompilerMulti files output = do
    result <- loadMultipleFiles files
    case result of
        Left err ->
            hPutStrLn stderr err >>
            exitWith (ExitFailure 84)
        Right prog ->
            -- Check that all called functions are defined
            case checkFunctionDefinitions prog of
                Left err ->
                    hPutStrLn stderr ("*** ERROR : " ++ err) >>
                    exitWith (ExitFailure 84)
                Right () ->
                    let llFile = output ++ ".ll"
                    in compileProgramToFile prog llFile >>
                       do
                           -- Call clang to compile the LLVM IR
                           (exitCode, _, clangErr) <-
                             readProcessWithExitCode "clang"
                               [llFile, "-o", output] ""
                           case exitCode of
                               ExitSuccess ->
                                   removeFile llFile >>
                                   return ()
                               ExitFailure _ ->
                                   (hPutStrLn stderr $
                                     "Clang compilation failed:\n" ++
                                     clangErr) >>
                                   exitWith (ExitFailure 84)
