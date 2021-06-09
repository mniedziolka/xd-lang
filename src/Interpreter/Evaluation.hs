module Interpreter.Evaluation where

import Control.Monad.Except
import Control.Monad.Reader

import qualified Data.Map.Lazy as Map

import Interpreter.Store
import Interpreter.Types
import Parser.Abs

assignFunctionArgs :: [Arg] -> [Expr] -> Env -> InterpreterMonad Env
assignFunctionArgs [] [] funEnv = do
  return funEnv
assignFunctionArgs ((VArg _ t ident):args) (e:es) funEnv = do
  newFunEnv <- local (const funEnv) $ alloc t ident
  value <- evalExpr e
  local (const newFunEnv) $ assignValue ident value
  assignFunctionArgs args es newFunEnv
assignFunctionArgs ((RArg _ t ident):args) ((EVar _ varIdent):es) funEnv = do
  newFunEnv <- local (const funEnv) $ alloc t ident
  loc <- getIdentLoc varIdent
  assignFunctionArgs args es (Map.insert ident loc newFunEnv)
assignFunctionArgs _ _ _ = throwError "Error: Unknown error"


evalExpr :: Expr -> InterpreterMonad Value
evalExpr (EVar _ ident) = getIdentValue ident
evalExpr (ELitInt _ val) = return $ VInt val
evalExpr (ELitTrue _) = return $ VBool True
evalExpr (ELitFalse _) = return $ VBool False

evalExpr (EApp (Just pos) ident argsExpr) = do
  VFun env funType args block <- getIdentValue ident
  envWithArgs <- assignFunctionArgs args argsExpr env
  retValue <- local (const envWithArgs) (evalBlock block)
  case retValue of
    Just (ReturnWithValue value) -> return value
    Just (Return) -> return VNull
    _ -> throwError ("Error: Function without yeet in " ++ show pos)

evalExpr (EString _ val) = return $ VString val
evalExpr (Neg _ expr) = do
  VInt int <- evalExpr expr
  return $ VInt (- int)

evalExpr (Not _ expr) = do
  VBool bool <- evalExpr expr
  return $ VBool (not bool)

evalExpr (EMul _ e1 (Times _) e2) = do
  VInt int1 <- evalExpr e1
  VInt int2 <- evalExpr e2
  return $ VInt (int1 * int2)

evalExpr (EMul _ e1 (Div (Just pos)) e2) = do
  VInt int1 <- evalExpr e1
  VInt int2 <- evalExpr e2
  if int2 == 0 then throwError $ "Error: Division by zero in " ++ show(pos)
  else return $ VInt (int1 `div` int2)

evalExpr (EMul _ e1 (Mod (Just pos)) e2) = do
  VInt int1 <- evalExpr e1
  VInt int2 <- evalExpr e2
  if int2 == 0 then throwError $ "Error: Modulo by zero in " ++ show(pos)
  else return $ VInt (int1 `mod` int2)

evalExpr (EAdd _ e1 (Plus _) e2) = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  case (v1, v2) of
    (VInt int1, VInt int2) -> return $ VInt (int1 + int2)
    (VString s1, VString s2) -> return $ VString (s1 ++ s2)
    (_, _) -> throwError "Error: Unknown error."

evalExpr (EAdd _ e1 (Minus _) e2) = do
  VInt int1 <- evalExpr e1
  VInt int2 <- evalExpr e2
  return $ VInt (int1 - int2)

evalExpr (ERel _ e1 op e2) = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  case op of
    LTH _ -> case (v1, v2) of
      (VInt i1, VInt i2) -> return $ VBool (i1 < i2)
      (_, _) -> throwError "Error: < operator wrong type"
    LE _ -> case (v1, v2) of
      (VInt i1, VInt i2) -> return $ VBool (i1 <= i2)
      (_, _) -> throwError "Error: <= operator wrong type"
    GTH _ -> case (v1, v2) of
      (VInt i1, VInt i2) -> return $ VBool (i1 > i2)
      (_, _) -> throwError "Error: > operator wrong type"
    GE _ -> case (v1, v2) of
      (VInt i1, VInt i2) -> return $ VBool (i1 >= i2)
      (_, _) -> throwError "Error: >= operator wrong type"
    EQU _ -> return $ VBool (v1 == v2)
    NE _ -> return $ VBool (v1 /= v2)

evalExpr (EAnd _ e1 e2) = do
  VBool b1 <- evalExpr e1
  VBool b2 <- evalExpr e2
  return $ VBool $ b1 && b2

evalExpr (EOr _ e1 e2) = do
  VBool b1 <- evalExpr e1
  VBool b2 <- evalExpr e2
  return $ VBool $ b1 || b2

evalBlock :: Block -> InterpreterMonad (Maybe StmtEnding)
evalBlock (Block _ []) = do
  return Nothing
evalBlock (Block _ (head:tail)) = do
  ending <- evalStmt head
  case ending of
    Nothing -> evalBlock (Block Nothing tail)
    Just Return -> return $ Just Return
    Just (ReturnWithValue value) -> return $ Just $ ReturnWithValue value
    Just LoopContinue -> return $ Just LoopContinue
    Just LoopBreak -> return $ Just LoopBreak
    Just (VEnv env) -> local (const env) (evalBlock (Block Nothing tail))

evalStmt :: Stmt -> InterpreterMonad (Maybe StmtEnding)
evalStmt (Empty _) = return Nothing
evalStmt (BStmt _ block) = do
  ending <- evalBlock block
  return ending

evalStmt (VarDecl _ t items) = do
  env <- putVarDecl t items
  return $ Just $ VEnv env

evalStmt (Ass _ ident expr) = do
  env <- ask
  val <- evalExpr expr
  assignValue ident val
  return $ Just $ VEnv env

evalStmt (Ret _) = do
  return $ Just Return

evalStmt (VRet _ expr) = do
  exprResult <- evalExpr expr
  return $ Just $ ReturnWithValue exprResult

evalStmt (Cond _ expr stmt) = do
  VBool val <- evalExpr expr
  if val then evalStmt stmt
  else return Nothing

evalStmt (CondElse _ expr stmt1 stmt2) = do
  VBool val <- evalExpr expr
  if val then evalStmt stmt1
  else evalStmt stmt2

evalStmt (While _ expr block) = do
  VBool b <- evalExpr expr
  if b then do
    ret <- evalStmt block
    case ret of
      Just LoopBreak -> return Nothing
      Just Return -> return $ Just Return
      Just (ReturnWithValue v) -> return $ Just $ ReturnWithValue v
      _ -> evalStmt $ While Nothing expr block
  else
    return Nothing

evalStmt (SExp _ expr) = do
  val <- evalExpr expr
  return Nothing

evalStmt (Break _) = do
  return $ Just LoopBreak

evalStmt (Continue _) = do
  return $ Just LoopContinue

evalStmt (FnDef _ retType ident args block) = do
  env <- alloc retType ident
  local (const env) (assignValue ident (VFun env retType args block))
  return $ Just $ VEnv env

evalStmt (Print _ expr) = do
  output <- evalExpr expr
  case output of
    VString s -> liftIO $ print s
    VInt x -> liftIO $ print x
    _ -> throwError "Unknown error"
  return Nothing


putVarDecl :: Type -> [Item] -> InterpreterMonad Env
putVarDecl _ [] = ask
putVarDecl t ((NoInit _ ident):tail) = do
  env <- alloc t ident
  local (const env) $ putVarDecl t tail

putVarDecl t ((Init _ ident expr):tail) = do
  env <- alloc t ident
  val <- evalExpr expr
  local (const env) $ assignValue ident val
  local (const env) $ putVarDecl t tail