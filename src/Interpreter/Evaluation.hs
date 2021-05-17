module Interpreter.Evaluation where

import Control.Monad.Except
import Control.Monad.Reader

import Interpreter.Store
import Interpreter.Types
import Parser.Abs

calculateArgsValues :: [Expr] -> InterpreterMonad [Value]
calculateArgsValues =
  mapM evalExpr

evalExpr :: Expr -> InterpreterMonad Value
evalExpr (EArrayVar _ x y) = undefined
evalExpr (EVar _ ident) = getIdentValue ident
evalExpr (ELitInt _ val) = return $ VInt val
evalExpr (ELitTrue _) = return $ VBool True
evalExpr (ELitFalse _) = return $ VBool False

evalExpr (EApp _ ident argsExpr) = do
  VFun env funType args block <- getIdentValue ident
  argsValues <- calculateArgsValues argsExpr
  envWithArgs <- local (const env) (assignFunctionArgs args argsValues)
  (retValue, _) <- local (const envWithArgs) (evalBlock block)
  case retValue of
    Just (ReturnWithValue value) -> return value
    _ -> throwError "Function without yeeeeet"

evalExpr (EString _ val) = return $ VString val
evalExpr (ETuple _ items) = undefined
evalExpr (Neg _ expr) = undefined
evalExpr (EMul _ e1 op e2) = undefined
evalExpr (EAdd _ e1 op e2) = undefined
evalExpr (ERel _ e1 op e2) = undefined
evalExpr (EAnd _ e1 e2) = undefined
evalExpr (EOr _ e1 e2) = undefined

evalBlock :: Block -> InterpreterMonad ((Maybe StmtEnding), Env)
evalBlock (Block _ []) = do
  env <- ask
  return (Nothing, env)
evalBlock (Block _ (head:tail)) = do
  (ending, env) <- evalStmt head
  case ending of
    Nothing -> local (const env) $ evalBlock (Block Nothing tail)
    Just Return -> return $ (Just Return, env)
    Just (ReturnWithValue value) -> return $ (Just (ReturnWithValue value), env)
    _ -> throwError "Break and Continue not supported"

evalStmt :: Stmt -> InterpreterMonad ((Maybe StmtEnding), Env)
evalStmt (Empty _) = undefined
evalStmt (BStmt _ block) = evalBlock block

evalStmt (VarDecl _ declType items) = do


evalStmt (Ass _ ident expr) = do
  env <- ask
  val <- evalExpr expr
  assignValue ident val
  return (Nothing, env)

evalStmt (Incr _ ident) = undefined
evalStmt (Decr _ ident) = undefined

evalStmt (Ret _) = do
  env <- ask
  return $ (Just Return, env)

evalStmt (VRet _ expr) = do
  env <- ask
  exprResult <- evalExpr expr
  return $ (Just $ ReturnWithValue exprResult, env)

evalStmt (Cond _ expr stmt) = undefined
evalStmt (CondElse _ expr stmt1 stmt2) = undefined
evalStmt (While _ expr block) = undefined
evalStmt (Break _) = undefined
evalStmt (ArrayAss _ _ _ _) = undefined
evalStmt (TupleUnpackExpr _ targets expr) = undefined
evalStmt (TupleUnpackIdent _ targets ident) = undefined
evalStmt (FnDef _ retType ident args block) = do
  env <- alloc retType ident
  local (const env) (assignValue ident (VFun env retType args block))
  return (Nothing, env)


