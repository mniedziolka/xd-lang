module TypeChecker.TypeCheck where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map.Lazy as Map

import Parser.Abs

import TypeChecker.Environment
import TypeChecker.Evaluation
import TypeChecker.Types

topDefinitions :: [Stmt] -> TypeCheckerMonad Env
topDefinitions [] = ask
topDefinitions (head:tail) = do
  stmtRet <- evalStmt head TCInt False
  case stmtRet of
    Just (TCEnv env) -> do
      local (const env) (topDefinitions tail)
    _ -> topDefinitions tail

typeCheck :: Program -> TypeCheckerMonad ()
typeCheck (Program pos topDefs) = do
  newEnv <- topDefinitions topDefs
  maybeMain <- local (const newEnv) $ askForType (Ident "main")
  case maybeMain of
    Nothing -> throwError "Error: There's no main function!"
    Just (TCFun (Int _) []) -> return ()
    _ -> throwError "Error: Main function should be int type and have no arguments."

runTypeCheck :: Program -> IO (Either String ())
runTypeCheck tree =
  runReaderT (runExceptT (typeCheck tree)) Map.empty
