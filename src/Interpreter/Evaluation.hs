module Interpreter.Evaluation where

import Interpreter.Types
import Parser.Abs

evalExpr :: Expr -> InterpreterMonad Value
evalExpr _ = do
  return (VInt 0)