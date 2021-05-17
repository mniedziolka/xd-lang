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
  retValue <- local (const envWithArgs) (evalBlock block)
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

evalBlock :: Block -> InterpreterMonad (Maybe StmtEnding)
evalBlock (Block _ []) = return Nothing
evalBlock (Block _ (head:tail)) = do
  ending <- evalStmt head
  case ending of
    Nothing -> evalBlock (Block Nothing tail)
    Just Return -> return $ Just Return
    Just (ReturnWithValue value) -> return $ Just (ReturnWithValue value)
    _ -> throwError "Break and Continue not supported"

evalStmt :: Stmt -> InterpreterMonad (Maybe StmtEnding)
evalStmt (Empty _) = undefined
evalStmt (BStmt _ block) = evalBlock block
evalStmt (Decl _ declType items) = undefined
evalStmt (Ass _ ident expr) = undefined
evalStmt (Incr _ ident) = undefined
evalStmt (Decr _ ident) = undefined

evalStmt (Ret _) = return $ Just Return

evalStmt (VRet _ expr) = do
  exprResult <- evalExpr expr
  return $ Just $ ReturnWithValue exprResult

evalStmt (Cond _ expr stmt) = undefined
evalStmt (CondElse _ expr stmt1 stmt2) = undefined
evalStmt (While _ expr block) = undefined
evalStmt (Break _) = undefined
evalStmt (ArrayAss _ _ _ _) = undefined
evalStmt (TupleUnpackExpr _ targets expr) = undefined
evalStmt (TupleUnpackIdent _ targets ident) = undefined


