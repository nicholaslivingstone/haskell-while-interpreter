module WhileLang.Interpreter.Runtime where

import WhileLang.Syntax
import WhileLang.Interpreter.Command (interpCommand)
import WhileLang.Interpreter.Boolean (evalBool)
import WhileLang.Interpreter.Arithmetic (evalArith)
import Control.Monad.State (StateT, runStateT)
import Control.Monad.Except (ExceptT, runExceptT)
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


runInterpreter :: Command -> IO (Either WhileError VarMap)
runInterpreter command = do
  (result, finalState) <- runStateT (runExceptT (interpCommand command)) emptyVarMap
  case result of
    Left err -> return (Left err)
    Right () -> return (Right finalState)


