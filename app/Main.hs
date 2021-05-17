module Main where

import Prelude
  ( ($)
  , Either(..)
  , Int, (>)
  , String, (++), unlines
  , Show, show
  , IO, (>>), (>>=), mapM_, putStrLn
  , FilePath
  , getContents, readFile
  )
import System.Environment ( getArgs )
import System.Exit        ( exitFailure, exitSuccess )
import Control.Monad      ( when )

import Parser.Abs   ( Program )
import Parser.Lex   ( Token )
import Parser.Par   ( pProgram, myLexer )
import Parser.Print ( Print, printTree )
import Parser.Skel  ()

import TypeChecker.TypeCheck (runTypeCheck)

import Interpreter.Program (runInterpreter)

type Err        = Either String
type ParseFun a = [Token] -> Err a
type Verbosity  = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: Verbosity -> ParseFun Program -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: Verbosity -> ParseFun Program -> String -> IO ()
run v p s =
  case p ts of
    Left err -> do
      putStrLn "\nParse              Failed...\n"
      putStrV v "Tokens:"
      putStrV v $ show ts
      putStrLn err
      exitFailure
    Right tree ->
      case runTypeCheck tree of
        Left error -> do
          putStrLn "\nParse Successful!"
          putStrLn "Typecheck fail"
          exitFailure
        Right _ -> do
          putStrLn "\nTypecheck Successful!"
--          showTree v tree
          (result, store) <- runInterpreter tree
          case result of
            Left error -> do
              putStrLn $ show error
              exitFailure
            Right exitCode -> do
              putStrLn $ "Exit code: " ++ show exitCode
              exitSuccess
  where
  ts = myLexer s

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree = do
  putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    []         -> getContents >>= run 2 pProgram
    "-s":fs    -> mapM_ (runFile 0 pProgram) fs
    fs         -> mapM_ (runFile 2 pProgram) fs

