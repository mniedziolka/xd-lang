module Interpreter.Types where

import qualified Data.Map.Lazy as Map

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Parser.Abs

data Value = VInt Integer
           | VString String
           | VBool Bool
           | VFun Env Type [Arg] Block
           | VNull
  deriving (Eq, Ord)

data StmtEnding = LoopBreak
                | LoopContinue
                | Return
                | ReturnWithValue Value

type Loc = Int
type Env = Map.Map Ident Loc
type Store = Map.Map Loc Value

type InterpreterMonad a = ExceptT String (ReaderT Env (StateT Store IO)) a
