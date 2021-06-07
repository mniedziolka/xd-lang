module TypeChecker.TypeCheck where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map.Lazy as Map

import Parser.Abs

import TypeChecker.Evaluation
import TypeChecker.Types

topDefinitions :: [Stmt] -> TypeCheckerMonad ()
topDefinitions [] = return ()
topDefinitions (head:tail) = do
  stmtRet <- evalStmt head TCInt False
  case stmtRet of
    Just (TCEnv env) -> do
      local (const env) (topDefinitions tail)
      return ()
    _ -> topDefinitions tail

typeCheck :: Program -> TypeCheckerMonad ()
typeCheck (Program pos topDefs) = do
  env <- topDefinitions topDefs
  return ()

runTypeCheck :: Program -> IO (Either String ())
runTypeCheck tree =
  runReaderT (runExceptT (typeCheck tree)) Map.empty
