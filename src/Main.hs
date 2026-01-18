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
import Interpreter (runProgram, runProgramFromFile, Value(..))
import Compiler (compileProgramToFile)
import System.IO (hPutStrLn, hPutStr, hFlush, hIsEOF, stderr, stdout, stdin)
import System.Process (readProcessWithExitCode)
import System.Directory (removeFile, doesFileExist)
import System.FilePath (dropExtension, takeDirectory, (</>))
import AST
import qualified Data.Set as Set
import Data.List (nub)
import Control.Monad (forM, foldM, when, unless)

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Just (Interpret file) -> runInterpreter file
        Just (Compile files output keepLL readable cabi) -> runCompilerMulti files output keepLL readable cabi
        Just Repl -> runRepl
        Nothing ->
            hPutStrLn stderr
              "Usage: glados                                     (interactive REPL)" >>
            hPutStrLn stderr
              "       glados -i <file>                           (interpret)" >>
            hPutStrLn stderr
              "       glados -c <files...> [-o out] [-ll] [-r] [-cabi]  (compile)" >>
            hPutStrLn stderr
              "  -ll   : keep the .ll (LLVM IR) file" >>
            hPutStrLn stderr
              "  -r    : generate readable .s (assembly) file" >>
            hPutStrLn stderr
              "  -cabi : generate C ABI compatible functions (no boxing)" >>
            exitWith (ExitFailure 84)

data Mode = Interpret FilePath | Compile [FilePath] FilePath Bool Bool Bool | Repl  -- files, output, keepLL, readable, cabi

parseArgs :: [String] -> Maybe Mode
parseArgs [] = Just Repl  -- No arguments = REPL mode
parseArgs ["-i", file] = Just (Interpret file)
parseArgs args | "-c" `elem` args =
    let cPos = length $ takeWhile (/= "-c") args
        afterC = drop (cPos + 1) args
        (beforeO, rest) = break (== "-o") afterC
        files = takeWhile (\x -> x /= "-o" && x /= "-ll" && x /= "-r" && x /= "-cabi") beforeO
        output = case break (== "-o") rest of
                  (_, ("-o":out:_)) -> out
                  _ -> case files of
                        (f:_) -> dropExtension f
                        [] -> "output"
        allFlags = drop 1 $ dropWhile (/= "-o") rest
        keepLL = "-ll" `elem` allFlags
        readable = "-r" `elem` allFlags
        cabi = "-cabi" `elem` allFlags
    in if null files then Nothing else Just (Compile files output keepLL readable cabi)
parseArgs _ = Nothing

runInterpreter :: FilePath -> IO ()
runInterpreter file = do
    input <- readFile file
    case P.parseProgram input of
        Left err ->
            hPutStrLn stderr (errorBundlePretty err) >>
            exitWith (ExitFailure 84)
        Right prog -> do
            r <- runProgramFromFile prog (takeDirectory file)
            case r of
                Left err ->
                    hPutStrLn stderr ("*** ERROR : " ++ err ++
                      if not (null err) && last err == '.'
                      then "" else ".") >>
                    exitWith (ExitFailure 84)
                Right () -> exitWith ExitSuccess

runRepl :: IO ()
runRepl = do
    putStrLn "Flux REPL - Interactive Mode"
    putStrLn "Type expressions or definitions, press Ctrl+D to exit"
    putStrLn "Multi-line input: lines ending with { will continue on next line"
    putStrLn ""
    replLoop []
  where
    replLoop :: Program -> IO ()
    replLoop env = readInput "" >>= processInput env
    
    -- Read input, handling multi-line constructs
    readInput :: String -> IO String
    readInput accumulated = do
        let prompt = if null accumulated then "flux> " else "...   "
        hPutStr stdout prompt
        hFlush stdout
        
        -- Check for EOF (Ctrl+D)
        eof <- hIsEOF stdin
        if eof
            then putStrLn "" >> exitWith ExitSuccess
            else do
                line <- getLine
                let newInput = if null accumulated 
                              then line 
                              else accumulated ++ "\n" ++ line
                
                -- Check if we need more input (unbalanced braces)
                if hasUnclosedBraces newInput
                    then readInput newInput
                    else return newInput
    
    -- Check if input has unclosed braces
    hasUnclosedBraces :: String -> Bool
    hasUnclosedBraces input = countBraces input /= 0
      where
        countBraces :: String -> Int
        countBraces = foldl count 0
          where
            count n '{' = n + 1
            count n '}' = n - 1
            count n _ = n
    
    processInput :: Program -> String -> IO ()
    processInput env input = do
        -- Skip empty input
        if null (filter (not . (`elem` " \t\n")) input)
            then replLoop env
            else do
                case P.parseProgram input of
                    Left err -> do
                        putStrLn $ "Parse error: " ++ errorBundlePretty err
                        replLoop env
                    Right newDefs -> do
                        let fullProg = env ++ newDefs
                        result <- runProgram fullProg
                        case result of
                            Left err -> do
                                putStrLn $ "Error: " ++ err
                                replLoop env
                            Right () -> do
                                replLoop fullProg

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
           [name | TLProc name _ _ <- prog] ++
           [name | TLLet name _ <- prog])  -- Include let bindings as possible callables
        calledFuncs = findCalledFuncs prog
        undefined = Set.filter (not . isBuiltin)
          (calledFuncs Set.\\ definedFuncs)
    in if Set.null undefined
        then Right ()
        else Left $
          "Undefined functions: " ++ show (Set.toList undefined)

-- | Find all function calls in a program, excluding known bound names
findCalledFuncs :: Program -> Set.Set String
findCalledFuncs prog =
  Set.fromList [name | TLExpr expr <- prog,
                       name <- findCallsInExpr Set.empty expr] <>
  Set.unions [Set.fromList (findCallsInExpr (Set.fromList params) body) |
              TLFn _ params body <- prog] <>
  Set.unions [Set.fromList (findCallsInTopLevel Set.empty top) |
              TLProc _ _ tops <- prog, top <- tops]

findCallsInExpr :: Set.Set String -> Expr -> [String]
findCallsInExpr bound (ECall (EVar name) args)
  | name `Set.member` bound = concatMap (findCallsInExpr bound) args
  | otherwise = name : concatMap (findCallsInExpr bound) args
findCallsInExpr bound (ECall expr args) =
  findCallsInExpr bound expr ++ concatMap (findCallsInExpr bound) args
findCallsInExpr bound (EBinary _ e1 e2) =
  findCallsInExpr bound e1 ++ findCallsInExpr bound e2
findCallsInExpr bound (EUnary _ e) = findCallsInExpr bound e
findCallsInExpr bound (EIf e1 e2 e3) =
  findCallsInExpr bound e1 ++ findCallsInExpr bound e2 ++ findCallsInExpr bound e3
findCallsInExpr bound (ELam params body) =
  findCallsInExpr (Set.union bound (Set.fromList params)) body
findCallsInExpr bound (EList es) = concatMap (findCallsInExpr bound) es
findCallsInExpr bound (ETuple es) = concatMap (findCallsInExpr bound) es
findCallsInExpr bound (EBlock tops mexpr) =
  let (newBound, calls) = foldl collectTop (bound, []) tops
  in calls ++ maybe [] (findCallsInExpr newBound) mexpr
  where
    collectTop (b, cs) (TLLet name expr) =
      (Set.insert name b, cs ++ findCallsInExpr b expr)
    collectTop (b, cs) (TLFn name params body) =
      (Set.insert name b, cs ++ findCallsInExpr (Set.union b (Set.fromList params)) body)
    collectTop (b, cs) (TLProc name params stmts) =
      let (b', cs') = foldl collectTop (Set.union b (Set.fromList params), []) stmts
      in (Set.insert name b', cs ++ cs')
    collectTop (b, cs) (TLExpr expr) = (b, cs ++ findCallsInExpr b expr)
    collectTop (b, cs) (TLImport _ _) = (b, cs)
findCallsInExpr bound (ESeq es) = concatMap (findCallsInExpr bound) es
findCallsInExpr _ _ = []

findCallsInTopLevel :: Set.Set String -> TopLevel -> [String]
findCallsInTopLevel bound (TLFn _ params body) =
  findCallsInExpr (Set.union bound (Set.fromList params)) body
findCallsInTopLevel bound (TLProc _ params tops) =
  let bound' = Set.union bound (Set.fromList params)
  in concatMap (findCallsInTopLevel bound') tops
findCallsInTopLevel bound (TLLet _ expr) = findCallsInExpr bound expr
findCallsInTopLevel bound (TLExpr expr) = findCallsInExpr bound expr
findCallsInTopLevel _ (TLImport _ _) = []

-- | Check if function is a builtin
isBuiltin :: String -> Bool
isBuiltin name = name `elem` builtins
  where
    builtins = ["print", "len", "concat", "substring", "charAt",
                "toUpper", "toLower", "abs", "min", "max", "pow",
                "isInt", "isBool", "isString", "isList",
                "readFile", "writeFile", "appendFile",
                "head", "tail", "at", "reverse"]

-- | Compile multiple files
runCompilerMulti :: [FilePath] -> FilePath -> Bool -> Bool -> Bool -> IO ()
runCompilerMulti files output keepLL readable cabi = do
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
                    in compileProgramToFile prog llFile cabi >>
                       do
                           if cabi then
                             -- In CABI mode, just compile to object file
                             do
                               (exitCode, _, clangErr) <-
                                 readProcessWithExitCode "clang"
                                   ["-c", llFile, "-o", output ++ ".o"] ""
                               case exitCode of
                                   ExitSuccess -> do
                                       -- Generate readable assembly if requested
                                       when readable $ do
                                         _ <- readProcessWithExitCode "clang"
                                           ["-S", llFile, "-o", output ++ ".s"] ""
                                         return ()
                                       -- Delete .ll file unless -ll flag was used
                                       unless keepLL $
                                         removeFile llFile
                                       return ()
                                   ExitFailure _ ->
                                       (hPutStrLn stderr $
                                         "Clang compilation failed:\n" ++
                                         clangErr) >>
                                       exitWith (ExitFailure 84)
                           else
                             -- Normal mode: link to executable
                             do
                               (exitCode, _, clangErr) <-
                                 readProcessWithExitCode "clang"
                                   [llFile, "-o", output] ""
                               case exitCode of
                                   ExitSuccess -> do
                                       -- Generate readable assembly if requested
                                       when readable $ do
                                         _ <- readProcessWithExitCode "clang"
                                           ["-S", llFile, "-o", output ++ ".s"] ""
                                         return ()
                                       -- Delete .ll file unless -ll flag was used
                                       unless keepLL $
                                         removeFile llFile
                                       return ()
                                   ExitFailure _ ->
                                       (hPutStrLn stderr $
                                         "Clang compilation failed:\n" ++
                                         clangErr) >>
                                       exitWith (ExitFailure 84)
