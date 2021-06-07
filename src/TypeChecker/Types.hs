module TypeChecker.Types where

import qualified Data.Map.Lazy as Map

import Control.Monad.Except
import Control.Monad.Reader

import Parser.Abs

data TCType = TCInt
            | TCString
            | TCBool
            | TCFun Type [Arg]
            | TCVoid
  deriving (Eq, Ord)

data StmtEnding = LoopBreak
                | LoopContinue
                | Return
                | ReturnWithValue TCType
                | TCEnv Env

type Env = Map.Map Ident TCType

type TypeCheckerMonad a = ExceptT String (ReaderT Env IO) a
