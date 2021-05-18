module Interpreter.Store where

import qualified Data.Map.Lazy as Map

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Interpreter.Types
import Parser.Abs

getIdentLoc :: Ident -> InterpreterMonad Loc
getIdentLoc ident = do
  loc <- ask
  case Map.lookup ident loc of
    Nothing ->
      throwError $ "Variable not found"
    Just location -> return location

getLocValue :: Loc -> InterpreterMonad Value
getLocValue loc = do
  store <- get
  let maybeValue = Map.lookup loc store in
    case maybeValue of
      Just VNull -> throwError "Error: Value not initialized."
      Just value -> return value
      Nothing -> throwError "Error: Value not found (TODO: TYPECHECK)."


getIdentValue :: Ident -> InterpreterMonad Value
getIdentValue ident = do
  loc <- getIdentLoc ident
  getLocValue loc

assignValue :: Ident -> Value -> InterpreterMonad ()
assignValue ident value = do
  store <- get
  loc <- getIdentLoc ident
  put $ Map.insert loc value store

alloc :: Type -> Ident -> InterpreterMonad Env
alloc varType ident = do
  store <- get
  let loc = Map.size store + 1 in do
    put (Map.insert loc VNull store)
    env <- ask
    return (Map.insert ident loc env)

assignFunctionArgs :: [Arg] -> [Value] -> [Maybe Loc] -> InterpreterMonad Env
assignFunctionArgs [] [] [] = do
  env <- ask
  return env
assignFunctionArgs ((VArg _ t ident):args) (v:vs) (l:ls) = do
  env <- alloc t ident
  local (const env) $ assignValue ident v
  local (const env) $ assignFunctionArgs args vs ls
assignFunctionArgs ((RArg _ t ident):args) (v:vs) ((Just loc):ls) = do
  env <- alloc t ident
  local (const $ Map.insert ident loc env) $ assignFunctionArgs args vs ls
assignFunctionArgs _ _ _ = throwError "TYPECHECKER TODO"
