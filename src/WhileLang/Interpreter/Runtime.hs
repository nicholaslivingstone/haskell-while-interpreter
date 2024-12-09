module WhileLang.Interpreter.Runtime where

import WhileLang.Interpreter.Core (VarMap, WhileError(..))
import Control.Monad.State (StateT)
import Control.Monad.Except (ExceptT)


-- The Magical Monad Stack for the Interpreter
-- (StateT VarMap IO) Allows IO actions while managing variable states
-- (ExceptT WhileError) Allows error propogation using custom error type
type InterpreterMonad = ExceptT WhileError (StateT VarMap IO)



