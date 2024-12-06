module WhileLang.Interpreter.Core where

import qualified Data.Map as Map

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

