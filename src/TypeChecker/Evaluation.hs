module TypeChecker.Evaluation where

import Control.Monad.Except
import Control.Monad.Reader

import qualified Data.Map.Lazy as Map

import TypeChecker.Environment
import TypeChecker.Types
import Parser.Abs


evalExpr :: Expr -> TypeCheckerMonad TCType
evalExpr (EVar _ ident) = undefined
evalExpr (ELitInt _ val) = return TCInt
evalExpr (ELitTrue _) = return TCBool
evalExpr (ELitFalse _) = return TCBool

evalExpr (EApp _ ident argsExpr) = undefined

evalExpr (EString _ val) = return TCString
evalExpr (Neg pos expr) = do
  eType <- evalExpr expr
  case eType of
    TCInt -> return  TCInt
    _ -> throwError ("Error: Negative of non integer in " ++ show pos)

evalExpr (Not pos expr) = do
  eType <- evalExpr expr
  case eType of
    TCBool -> return  TCBool
    _ -> throwError ("Error: Negation of non bool type in " ++ show pos)

evalExpr (EMul _ e1 (Times pos) e2) = do
  e1Type <- evalExpr e1
  e2Type <- evalExpr e2
  case (e1Type, e2Type) of
    (TCInt, TCInt) -> return TCInt
    (TCString, TCString) -> throwError ("Error: Operator * not defined on Strings in " ++ show pos)
    (TCBool, TCBool) -> throwError ("Error: Operator * not defined on Bools in " ++ show pos)
    (_, _) -> throwError ("Error: Different types for operator * in " ++ show pos)

evalExpr (EMul _ e1 (Div pos) e2) = do
  e1Type <- evalExpr e1
  e2Type <- evalExpr e2
  case (e1Type, e2Type) of
    (TCInt, TCInt) -> return TCInt
    (TCString, TCString) -> throwError ("Error: Operator / is not defined for Strings in " ++ show pos)
    (TCBool, TCBool) -> throwError ("Error: Operator / is not defined for Bools in " ++ show pos)
    (_, _) -> throwError ("Error: Different types for operator / in " ++ show pos)

evalExpr (EMul _ e1 (Mod pos) e2) = do
  e1Type <- evalExpr e1
  e2Type <- evalExpr e2
  case (e1Type, e2Type) of
    (TCInt, TCInt) -> return TCInt
    (TCString, TCString) -> throwError ("Error: Operator % is not defined for Strings in " ++ show pos)
    (TCBool, TCBool) -> throwError ("Error: Operator % is not defined for Bools in " ++ show pos)
    (_, _) -> throwError ("Error: Different types for operator % in " ++ show pos)

evalExpr (EAdd _ e1 (Plus pos) e2) = do
  e1Type <- evalExpr e1
  e2Type <- evalExpr e2
  case (e1Type, e2Type) of
    (TCInt, TCInt) -> return TCInt
    (TCString, TCString) -> return TCString
    (TCBool, TCBool) -> throwError ("Error: Operator + is not defined for Bools in " ++ show pos)
    (_, _) -> throwError ("Error: Different types for operator + in " ++ show pos)

evalExpr (EAdd _ e1 (Minus pos) e2) = do
  e1Type <- evalExpr e1
  e2Type <- evalExpr e2
  case (e1Type, e2Type) of
    (TCInt, TCInt) -> return TCInt
    (TCString, TCString) -> throwError ("Error: Operator - is not defined for Strings in " ++ show pos)
    (TCBool, TCBool) -> throwError ("Error: Operator - is not defined for Bools in " ++ show pos)
    (_, _) -> throwError ("Error: Different types for operator - in " ++ show pos)

evalExpr (ERel _ e1 op e2) = undefined

evalExpr (EAnd pos e1 e2) = do
  e1Type <- evalExpr e1
  e2Type <- evalExpr e2
  case (e1Type, e2Type) of
    (TCBool, TCBool) -> return TCBool
    (TCInt, TCInt) -> throwError ("Error: Operator 'and' is not defined for Ints in " ++ show pos)
    (TCString, TCString) -> throwError ("Error: Operator 'and' is not defined for Strings in " ++ show pos)
    (_, _) -> throwError ("Error: Different types for operator 'and' in " ++ show pos)

evalExpr (EOr pos e1 e2) = do
  e1Type <- evalExpr e1
  e2Type <- evalExpr e2
  case (e1Type, e2Type) of
    (TCBool, TCBool) -> return TCBool
    (TCInt, TCInt) -> throwError ("Error: Operator 'or' is not defined for Ints in " ++ show pos)
    (TCString, TCString) -> throwError ("Error: Operator 'or' is not defined for Strings in " ++ show pos)
    (_, _) -> throwError ("Error: Different types for operator 'or' in " ++ show pos)

evalBlock :: Block -> TCType -> Bool -> TypeCheckerMonad (Maybe StmtEnding)
evalBlock (Block _ []) _ _ =
  return Nothing
evalBlock (Block pos (head:tail)) retType isWhile = do
  ending <- evalStmt head retType isWhile
  case ending of
    Nothing -> evalBlock (Block pos tail) retType isWhile
    Just Return ->
      case retType of
        TCVoid -> evalBlock (Block pos tail) retType isWhile
        _ -> throwError ("Error: Function is expecting type, caused by yeet in " ++ show pos)
    Just (ReturnWithValue valueType) ->
      if sameType retType valueType then
        evalBlock (Block pos tail) retType isWhile
      else
        throwError ("Error: type mismatch in function coused by yeet in " ++ show pos)
    Just LoopContinue ->
      if isWhile then evalBlock (Block pos tail) retType isWhile
      else throwError ("Error: Continue run outside while loop in " ++ show pos)
    Just LoopBreak ->
      if isWhile then evalBlock (Block pos tail) retType isWhile
      else throwError ("Error: Continue run outside while loop in " ++ show pos)
    Just (TCEnv env) -> local (const env) (evalBlock (Block pos tail) retType isWhile)

evalStmt :: Stmt -> TCType -> Bool -> TypeCheckerMonad (Maybe StmtEnding)
evalStmt (Empty _) _ _ = return Nothing
evalStmt (BStmt _ block) retType isWhile = do -- TODO CHeck this
  ending <- evalBlock block retType isWhile
  return ending

evalStmt (VarDecl _ t items) retType isWhile = do
  env <- putVarDecl t items
  return $ Just $ TCEnv env

evalStmt (Ass _ ident expr) retType isWhile = undefined

evalStmt (Incr _ ident) retType isWhile = undefined
evalStmt (Decr _ ident) retType isWhile = undefined

evalStmt (Ret _) retType isWhile = undefined

evalStmt (VRet _ expr) retType isWhile = undefined

evalStmt (Cond _ expr stmt) retType isWhile = undefined

evalStmt (CondElse _ expr stmt1 stmt2) retType isWhile = undefined

evalStmt (While pos expr block) retType _  = do
  eType <- evalExpr expr
  case eType of
    TCBool -> do
      ending <- evalStmt block retType True
      return Nothing
    _ -> throwError ("Error: Condition in while is not Bool in " ++ show pos)

evalStmt (SExp _ expr) retType isWhile = undefined

evalStmt (Break _) _ _ = return $ Just LoopBreak

evalStmt (Continue _) _ _ = return $ Just LoopContinue

evalStmt (FnDef _ funType ident args block) retType isWhile = undefined

evalStmt (Print pos expr) _ _ = do
  eType <- evalExpr expr
  case eType of
    TCInt -> return Nothing
    TCString -> return Nothing
    _ -> throwError ("Error: Unsupported type for print in " ++ show pos)

-- Int a | Str a | Bool a | Void a | Fun a (Type' a) [Type' a]
mapType :: Type -> TypeCheckerMonad TCType
mapType (Int _) = return TCInt
mapType (Str _) = return TCString
mapType (Bool _) = return TCBool
mapType (Void _) = return TCVoid
mapType _ = throwError "Strange"

checkType :: Type -> TCType -> Bool
checkType parserType tcType =
  case (parserType, tcType) of
      (Int _, TCInt) -> True
      (Str _, TCString) -> True
      (Bool _, TCBool) -> True
      (_, _) -> False

sameType :: TCType -> TCType -> Bool
sameType t1 t2 =
  case (t1, t2) of
    (TCInt, TCInt) -> True
    (TCString, TCString) -> True
    (TCBool, TCBool) -> True
    (_, _) -> False

putVarDecl :: Type -> [Item] -> TypeCheckerMonad Env
putVarDecl _ [] = ask
putVarDecl t ((NoInit pos ident):tail) = do
  tcType <- mapType t
  env <- assignType ident tcType
  local (const env) $ putVarDecl t tail

putVarDecl t ((Init pos ident expr):tail) = do
  valueType <- evalExpr expr
  if (checkType t valueType) then do
    env <- assignType ident valueType
    local (const env) $ putVarDecl t tail
  else
    throwError ("Error: Assigment of different types in " ++ show pos)
