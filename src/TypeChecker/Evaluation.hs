module TypeChecker.Evaluation where

import Control.Monad.Except
import Control.Monad.Reader

import qualified Data.Map.Lazy as Map

import TypeChecker.Environment
import TypeChecker.Types
import Parser.Abs


evalExpr :: Expr -> TypeCheckerMonad TCType
evalExpr (EVar (Just pos) ident) = do
  maybeTCType <- askForType ident
  case maybeTCType of
    Nothing -> throwError ("Error: Variable undefined in " ++ show pos)
    Just (TCFun _ _) -> throwError ("Error: Variable is function in " ++ show pos)
    Just tcType -> return tcType
evalExpr (ELitInt _ val) = return TCInt
evalExpr (ELitTrue _) = return TCBool
evalExpr (ELitFalse _) = return TCBool

evalExpr (EApp (Just pos) ident argsExpr) = do
  maybeTCType <- askForType ident
  case maybeTCType of
    Nothing -> throwError ("Error: Call of undeclared function in " ++ show pos)
    Just tcType ->
      case tcType of
        (TCFun funType argsType) -> do
          _ <- validateArgs argsType argsExpr
          retType <- mapType funType
          return retType
        _ -> throwError ("Error: Object is not callable in " ++ show pos)

evalExpr (EString _ val) = return TCString
evalExpr (Neg (Just pos) expr) = do
  eType <- evalExpr expr
  case eType of
    TCInt -> return TCInt
    _ -> throwError ("Error: Negative of non integer in " ++ show pos)

evalExpr (Not (Just pos) expr) = do
  eType <- evalExpr expr
  case eType of
    TCBool -> return  TCBool
    _ -> throwError ("Error: Negation of non bool type in " ++ show pos)

evalExpr (EMul _ e1 (Times (Just pos)) e2) = do
  e1Type <- evalExpr e1
  e2Type <- evalExpr e2
  case (e1Type, e2Type) of
    (TCInt, TCInt) -> return TCInt
    (TCString, TCString) -> throwError ("Error: Operator * not defined on Strings in " ++ show pos)
    (TCBool, TCBool) -> throwError ("Error: Operator * not defined on Bools in " ++ show pos)
    (_, _) -> throwError ("Error: Different types for operator * in " ++ show pos)

evalExpr (EMul _ e1 (Div (Just pos)) e2) = do
  e1Type <- evalExpr e1
  e2Type <- evalExpr e2
  case (e1Type, e2Type) of
    (TCInt, TCInt) -> return TCInt
    (TCString, TCString) -> throwError ("Error: Operator / is not defined for Strings in " ++ show pos)
    (TCBool, TCBool) -> throwError ("Error: Operator / is not defined for Bools in " ++ show pos)
    (_, _) -> throwError ("Error: Different types for operator / in " ++ show pos)

evalExpr (EMul _ e1 (Mod (Just pos)) e2) = do
  e1Type <- evalExpr e1
  e2Type <- evalExpr e2
  case (e1Type, e2Type) of
    (TCInt, TCInt) -> return TCInt
    (TCString, TCString) -> throwError ("Error: Operator % is not defined for Strings in " ++ show pos)
    (TCBool, TCBool) -> throwError ("Error: Operator % is not defined for Bools in " ++ show pos)
    (_, _) -> throwError ("Error: Different types for operator % in " ++ show pos)

evalExpr (EAdd _ e1 (Plus (Just pos)) e2) = do
  e1Type <- evalExpr e1
  e2Type <- evalExpr e2
  case (e1Type, e2Type) of
    (TCInt, TCInt) -> return TCInt
    (TCString, TCString) -> return TCString
    (TCBool, TCBool) -> throwError ("Error: Operator + is not defined for Bools in " ++ show pos)
    (_, _) -> throwError ("Error: Different types for operator + in " ++ show pos)

evalExpr (EAdd _ e1 (Minus (Just pos)) e2) = do
  e1Type <- evalExpr e1
  e2Type <- evalExpr e2
  case (e1Type, e2Type) of
    (TCInt, TCInt) -> return TCInt
    (TCString, TCString) -> throwError ("Error: Operator - is not defined for Strings in " ++ show pos)
    (TCBool, TCBool) -> throwError ("Error: Operator - is not defined for Bools in " ++ show pos)
    (_, _) -> throwError ("Error: Different types for operator - in " ++ show pos)

evalExpr (ERel (Just pos) e1 op e2) = do
  e1Type <- evalExpr e1
  e2Type <- evalExpr e2
  case op of
    LTH _ ->
      case (e1Type, e2Type) of
        (TCInt, TCInt) -> return TCBool
        (_, _) -> throwError ("Error: Undefined relation for types in " ++ show pos)
    LE _ ->
      case (e1Type, e2Type) of
        (TCInt, TCInt) -> return TCBool
        (_, _) -> throwError ("Error: Undefined relation for types in " ++ show pos)
    GTH _ ->
      case (e1Type, e2Type) of
        (TCInt, TCInt) -> return TCBool
        (_, _) -> throwError ("Error: Undefined relation for types in " ++ show pos)
    GE _ ->
      case (e1Type, e2Type) of
        (TCInt, TCInt) -> return TCBool
        (_, _) -> throwError ("Error: Undefined relation for types in " ++ show pos)
    EQU _ ->
      case (e1Type, e2Type) of
        (TCInt, TCInt) -> return TCBool
        (TCString, TCString) -> return TCBool
        (TCBool, TCBool) -> return TCBool
        (_, _) -> throwError ("Error: Undefined relation for types in " ++ show pos)
    NE _ ->
      case (e1Type, e2Type) of
        (TCInt, TCInt) -> return TCBool
        (TCString, TCString) -> return TCBool
        (TCBool, TCBool) -> return TCBool
        (_, _) -> throwError ("Error: Undefined relation for types in " ++ show pos)

evalExpr (EAnd (Just pos) e1 e2) = do
  e1Type <- evalExpr e1
  e2Type <- evalExpr e2
  case (e1Type, e2Type) of
    (TCBool, TCBool) -> return TCBool
    (TCInt, TCInt) -> throwError ("Error: Operator 'and' is not defined for Ints in " ++ show pos)
    (TCString, TCString) -> throwError ("Error: Operator 'and' is not defined for Strings in " ++ show pos)
    (_, _) -> throwError ("Error: Different types for operator 'and' in " ++ show pos)

evalExpr (EOr (Just pos) e1 e2) = do
  e1Type <- evalExpr e1
  e2Type <- evalExpr e2
  case (e1Type, e2Type) of
    (TCBool, TCBool) -> return TCBool
    (TCInt, TCInt) -> throwError ("Error: Operator 'or' is not defined for Ints in " ++ show pos)
    (TCString, TCString) -> throwError ("Error: Operator 'or' is not defined for Strings in " ++ show pos)
    (_, _) -> throwError ("Error: Different types for operator 'or' in " ++ show pos)

validateArgs :: [Arg] -> [Expr] -> TypeCheckerMonad Bool
validateArgs [] [] = return True
validateArgs (t:ts) (e:es) = do
  valueType <- evalExpr e
  case t of
    (VArg (Just pos) t ident) -> do
      argType <- mapType t
      if sameType argType valueType then
        validateArgs ts es
      else
        throwError ("Error: Type mismatch in function call in " ++ show pos)
    (RArg (Just pos) t ident) -> do
      argType <- mapType t
      if sameType argType valueType then
        validateArgs ts es
      else
        throwError ("Error: Type mismatch in function call in " ++ show pos)


evalBlock :: Block -> TCType -> Bool -> TypeCheckerMonad (Maybe StmtEnding)
evalBlock (Block _ []) _ _ =
  return Nothing
evalBlock (Block (Just pos) (head:tail)) retType isWhile = do
  ending <- evalStmt head retType isWhile
  case ending of
    Nothing -> evalBlock (Block (Just pos) tail) retType isWhile
    Just Return ->
      case retType of
        TCVoid -> evalBlock (Block (Just pos) tail) retType isWhile
        _ -> throwError ("Error: Function is expecting type, caused by yeet in " ++ show pos)
    Just (ReturnWithValue valueType) ->
      if sameType retType valueType then
        evalBlock (Block (Just pos) tail) retType isWhile
      else
        throwError ("Error: type mismatch in function coused by yeet in " ++ show pos)
    Just LoopContinue ->
      if isWhile then evalBlock (Block (Just pos) tail) retType isWhile
      else throwError ("Error: Continue run outside while loop in " ++ show pos)
    Just LoopBreak ->
      if isWhile then evalBlock (Block (Just pos) tail) retType isWhile
      else throwError ("Error: Break run outside while loop in " ++ show pos)
    Just (TCEnv env) -> local (const env) (evalBlock (Block (Just pos) tail) retType isWhile)

evalStmt :: Stmt -> TCType -> Bool -> TypeCheckerMonad (Maybe StmtEnding)
evalStmt (Empty _) _ _ = return Nothing
evalStmt (BStmt _ block) retType isWhile = do -- TODO CHeck this
  ending <- evalBlock block retType isWhile
  return Nothing

evalStmt (VarDecl _ t items) retType isWhile = do
  env <- putVarDecl t items
  return $ Just $ TCEnv env

evalStmt (Ass (Just pos) ident expr) _ _ = do
  env <- ask
  valType <- evalExpr expr
  maybeOriginalType <- askForType ident
  case maybeOriginalType of
    Nothing -> throwError ("Error: Assigment before definition in " ++ show pos)
    Just originalType ->
      if not (sameType valType originalType) then
        throwError ("Error: Assigment to a variable with different type in " ++ show pos)
      else
        return $ Just $ TCEnv env

evalStmt (Ret _) _ _ = return $ Just Return

evalStmt (VRet _ expr) _ _ = do
  valueType <- evalExpr expr
  return $ Just $ ReturnWithValue valueType

evalStmt (Cond (Just pos) expr stmt) retType isWhile = do
  condType <- evalExpr expr
  case condType of
    TCBool -> do
      ending <- evalStmt stmt retType isWhile
      return Nothing
    _ -> throwError ("Error: Expression in condition must be a Bool type in " ++ show pos)

evalStmt (CondElse (Just pos) expr stmt1 stmt2) retType isWhile = do
  condType <- evalExpr expr
  case condType of
    TCBool -> do
      endingTrue <- evalStmt stmt1 retType isWhile
      endingFalse <- evalStmt stmt2 retType isWhile
      return Nothing
    _ -> throwError ("Error: Expression in condition must be a Bool type in " ++ show pos)

evalStmt (While (Just pos) expr block) retType _  = do
  eType <- evalExpr expr
  case eType of
    TCBool -> do
      ending <- evalStmt block retType True
      return Nothing
    _ -> throwError ("Error: Condition in while is not Bool in " ++ show pos)

evalStmt (SExp _ expr) retType isWhile = do
  eType <- evalExpr expr
  return Nothing

evalStmt (Break _) _ _ = return $ Just LoopBreak

evalStmt (Continue _) _ _ = return $ Just LoopContinue

evalStmt (FnDef _ funType ident args block) _ _ = do
  env <- assignType ident (TCFun funType args)
  envWithArgs <- local (const env) (insertArgs args)
  tcType <- mapType funType
  _ <- local (const envWithArgs) (evalBlock block tcType False)
  return $ Just $ TCEnv env

evalStmt (Print (Just pos) expr) _ _ = do
  eType <- evalExpr expr
  case eType of
    TCInt -> return Nothing
    TCString -> return Nothing
    _ -> throwError ("Error: Unsupported type for print in " ++ show pos)

insertArgs :: [Arg] -> TypeCheckerMonad Env
insertArgs [] = ask
insertArgs ((VArg _ t ident):tail) = do
  env <- ask
  tcType <- mapType t
  envWithVariable <- local (const env) $ assignType ident tcType
  local (const envWithVariable) $ insertArgs tail
insertArgs ((RArg _ t ident):tail) = do
  env <- ask
  tcType <- mapType t
  envWithVariable <- local (const env) $ assignType ident tcType
  local (const envWithVariable) $ insertArgs tail

-- Int a | Str a | Bool a | Void a | Fun a (Type' a) [Type' a]
mapType :: Type -> TypeCheckerMonad TCType
mapType (Int _) = return TCInt
mapType (Str _) = return TCString
mapType (Bool _) = return TCBool
mapType (Void _) = return TCVoid
mapType _ = throwError "Error: Unknown error"

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
putVarDecl t ((NoInit _ ident):tail) = do
  tcType <- mapType t
  env <- assignType ident tcType
  local (const env) $ putVarDecl t tail

putVarDecl t ((Init (Just pos) ident expr):tail) = do
  valueType <- evalExpr expr
  if (checkType t valueType) then do
    env <- assignType ident valueType
    local (const env) $ putVarDecl t tail
  else
    throwError ("Error: Assigment of different types in " ++ show pos)
