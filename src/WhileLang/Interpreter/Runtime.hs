module WhileLang.Interpreter.Runtime where

import Control.Monad.State (StateT)
import Control.Monad.Except (ExceptT)
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

-- The Magical Monad Stack for the Interpreter
-- (StateT VarMap IO) Allows IO actions while managing variable states
-- (ExceptT WhileError) Allows error propogation using custom error type
type InterpreterMonad = ExceptT WhileError (StateT VarMap IO)






