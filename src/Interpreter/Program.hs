module Interpreter.Program where

import qualified Data.Map.Lazy as Map

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Interpreter.Evaluation
import Interpreter.Store
import Interpreter.Types
import Parser.Abs


topDefinition :: TopDef -> InterpreterMonad Env
topDefinition (FnDef position retType ident args block) = do
  env <- alloc retType ident
  local (const env) (assignValue ident (VFun env retType args block))
  return env

topDefinitions :: [TopDef] -> InterpreterMonad Env
topDefinitions [] = ask
topDefinitions (head:tail) = do
  env <- topDefinition head
  local (const env) (topDefinitions tail)


interpret :: Program -> InterpreterMonad Integer
interpret (Program position topDefs) = do
  env <- topDefinitions topDefs
  VInt exitCode <- local (const env) $ evalExpr $ EApp Nothing (Ident "main") []
  return exitCode


runInterpreter :: Program -> IO (Either String Integer, Store)
runInterpreter tree =
  runStateT (runReaderT (runExceptT (interpret tree)) Map.empty) Map.empty