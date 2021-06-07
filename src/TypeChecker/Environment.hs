module TypeChecker.Environment where

import qualified Data.Map.Lazy as Map

import Control.Monad.Except
import Control.Monad.Reader

import TypeChecker.Types
import Parser.Abs

assignType :: Ident -> TCType -> TypeCheckerMonad Env
assignType ident valueType = do
  env <- ask
  return $ Map.insert ident valueType env
