module Interpreter.WhileInterpreter where

import qualified Data.Map as Map

-- Program State
data ProgState = 
  ProgState { 
    varMap :: VarMap,
    err :: Maybe String,
    done :: Bool
  }

-- Variables
type VarName = String

type VarMap = Map.Map VarName Int

emptyVarMap :: VarMap
emptyVarMap = Map.empty

-- Errors
data WhileError
  = VarNotFound VarName
  | DivByZero
  | OtherError String
  deriving (Show, Eq)

