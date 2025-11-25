module Main (main) where

import System.IO (hGetContents, stdin, hPutStrLn, stderr)
import System.Exit (exitWith, ExitCode(..))
import Data.List (intercalate)
import Parser.LISP (parseProgram)
import Evaluator (initialEnv, evalProgram, showValue, containsUnbound)

main :: IO ()
main = runLispMode

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

whenNotNull :: [a] -> IO () -> IO ()
whenNotNull [] _ = pure ()
whenNotNull _  f = f

formatErr :: String -> String
formatErr s = "*** ERROR : " ++ fixDot s
    where
        fixDot str = if null str then str else if last str == '.' then str else str ++ "."