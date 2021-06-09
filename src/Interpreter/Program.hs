module Interpreter.Program where

import qualified Data.Map.Lazy as Map

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Interpreter.Evaluation
import Interpreter.Store
import Interpreter.Types
import Parser.Abs


topDefinitions :: [Stmt] -> InterpreterMonad Env
topDefinitions [] = ask
topDefinitions (head:tail) = do
  stmtRet <- evalStmt head
  case stmtRet of
    Just (VEnv env) ->  local (const env) (topDefinitions tail)
    _ -> topDefinitions tail


interpret :: Program -> InterpreterMonad Integer
interpret (Program pos topDefs) = do
  env <- topDefinitions topDefs
  VInt exitCode <- local (const env) $ evalExpr $ EApp pos (Ident "main") []
  return exitCode


runInterpreter :: Program -> IO (Either String Integer, Store)
runInterpreter tree =
  runStateT (runReaderT (runExceptT (interpret tree)) Map.empty) Map.empty