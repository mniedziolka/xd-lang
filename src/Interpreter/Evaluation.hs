module Interpreter.Evaluation where

import Control.Monad.Except

import Interpreter.Store
import Interpreter.Types
import Parser.Abs

evalExpr :: Expr -> InterpreterMonad Value
evalExpr (EArrayVar _ x y) = undefined
evalExpr (EVar _ ident) = getIdentValue ident
evalExpr (ELitInt _ val) = return $ VInt val
evalExpr (ELitTrue _) = return $ VBool True
evalExpr (ELitFalse _) = return $ VBool False
evalExpr (EApp _ ident argsExpr) = do
--  VFun env args block <- getIdentValue ident
  undefined
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
evalStmt (BStmt _ block) = undefined
evalStmt (Decl _ declType items) = undefined
evalStmt (Ass _ ident expr) = undefined
evalStmt (Incr _ ident) = undefined
evalStmt (Decr _ ident) = undefined
evalStmt (Ret _ expr) = undefined
evalStmt (VRet _ expr) = undefined
evalStmt (Ret _ expr) = undefined
evalStmt (Ret _ expr) = undefined
evalStmt (Ret _ expr) = undefined

