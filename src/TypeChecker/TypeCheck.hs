module TypeChecker.TypeCheck where

import Control.Monad.Except
import Control.Monad.Reader

import Parser.Abs ( Program )

--type TypeEnv = Map.Map VariableName Type



--runTypeCheck :: Program -> Err Int
runTypeCheck tree =
  Right 1