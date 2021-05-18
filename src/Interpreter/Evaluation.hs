module Interpreter.Evaluation where

import Control.Monad.Except
import Control.Monad.Reader

import Interpreter.Store
import Interpreter.Types
import Parser.Abs

calculateArgsValues :: [Expr] -> InterpreterMonad [Value]
calculateArgsValues =
  mapM evalExpr

getLoc :: [Expr] -> InterpreterMonad [Maybe Loc]
getLoc [] = return []
getLoc ((EVar _ ident):tail) = do
  loc <- getIdentLoc ident
  tailList <- getLoc tail
  return $ (Just loc):tailList
getLoc (_:tail) = do
  tailList <- getLoc tail
  return $ Nothing:tailList


evalExpr :: Expr -> InterpreterMonad Value
evalExpr (EArrayVar _ x y) = undefined
evalExpr (EVar _ ident) = getIdentValue ident
evalExpr (ELitInt _ val) = return $ VInt val
evalExpr (ELitTrue _) = return $ VBool True
evalExpr (ELitFalse _) = return $ VBool False

evalExpr (EApp _ ident argsExpr) = do
  VFun env funType args block <- getIdentValue ident
  argsValues <- calculateArgsValues argsExpr
  maybeLoc <- getLoc argsExpr
  envWithArgs <- local (const env) (assignFunctionArgs args argsValues maybeLoc)
  (retValue, _) <- local (const envWithArgs) (evalBlock block)
  case retValue of
    Just (ReturnWithValue value) -> return value
    Just (Return) -> return VNull
    _ -> throwError "Function without yeeeeet"

evalExpr (EString _ val) = return $ VString val
evalExpr (ETuple _ items) = undefined
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

evalExpr (EMul _ e1 (Div _) e2) = do
  VInt int1 <- evalExpr e1
  VInt int2 <- evalExpr e2
  if int2 == 0 then throwError "Error: Division by zero."
  else return $ VInt (int1 `div` int2)

evalExpr (EMul _ e1 (Mod _) e2) = do
  VInt int1 <- evalExpr e1
  VInt int2 <- evalExpr e2
  if int2 == 0 then throwError "Error: Modulo by zero."
  else return $ VInt (int1 `mod` int2)

evalExpr (EAdd _ e1 (Plus _) e2) = do
  VInt int1 <- evalExpr e1
  VInt int2 <- evalExpr e2
  return $ VInt (int1 + int2)

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
    Just LoopContinue -> return (Just LoopContinue, env)
    Just LoopBreak -> return (Just LoopBreak, env)

evalStmt :: Stmt -> InterpreterMonad ((Maybe StmtEnding), Env)
evalStmt (Empty _) = undefined
evalStmt (BStmt _ block) = do
  env <- ask
  (ending, _) <- evalBlock block
  return (ending, env)

evalStmt (VarDecl _ t items) = do
  env <- putVarDecl t items
  return (Nothing, env)

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

evalStmt (Cond _ expr stmt) = do
  env <- ask
  VBool val <- evalExpr expr
  if val then evalStmt stmt
  else return (Nothing, env)

evalStmt (CondElse _ expr stmt1 stmt2) = do
  env <- ask
  VBool val <- evalExpr expr
  if val then evalStmt stmt1
  else evalStmt stmt2

evalStmt (While _ expr block) = do
  env <- ask
  VBool b <- evalExpr expr
  if b then do
    (ret, _) <- evalStmt block
    case ret of
      Just LoopBreak -> return (Nothing, env)
      Just Return -> return $ (Just Return, env)
      Just (ReturnWithValue v) -> return $ (Just $ ReturnWithValue v, env)
      _ -> evalStmt $ While Nothing expr block
  else
    return (Nothing, env)

evalStmt (SExp _ expr) = do
  env <- ask
  val <- evalExpr expr
  return $ (Nothing, env)

evalStmt (Break _) = do
  env <- ask
  return (Just LoopBreak, env)

evalStmt (Continue _) = do
  env <- ask
  return (Just LoopContinue, env)

evalStmt (ArrayAss _ _ _ _) = undefined
evalStmt (TupleUnpackExpr _ targets expr) = undefined
evalStmt (TupleUnpackIdent _ targets ident) = undefined

evalStmt (FnDef _ retType ident args block) = do
  env <- alloc retType ident
  local (const env) (assignValue ident (VFun env retType args block))
  return (Nothing, env)

evalStmt (Print _ expr) = do
  env <- ask
  VString output <- evalExpr expr
  liftIO $ print output
  return (Nothing, env)


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