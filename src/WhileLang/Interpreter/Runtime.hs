module WhileLang.Interpreter.Runtime where

import WhileLang.Syntax 
import WhileLang.Interpreter.Boolean
import WhileLang.Interpreter.Arithmetic (interpArith)
import WhileLang.Interpreter.Core (VarMap, WhileError(..))
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map


-- The Magical Monad Stack for the Interpreter
-- (StateT VarMap IO) Allows IO actions while managing variable states
-- (ExceptT WhileError) Allows error propogation using custom error type
type InterpreterMonad = ExceptT WhileError (StateT VarMap IO)

-- Command Interpretation
interpCommand :: Command -> InterpreterMonad ()
interpCommand (Assign var expr) = do
  val <- interpArith expr
  modify (Map.insert var val)

interpCommand Skip = return ()

interpCommand (If cond thenCmd elseCmd) = do
  p <- interpBool cond
  let targetCmd = if p then thenCmd else elseCmd
  interpCommand targetCmd

interpCommand (While cond cmd) = do
  p <- interpBool cond
  return ()

interpCommand (Seq cmds) = mapM_ interpCommand cmds

interpCommand (Print str) = liftIO (putStrLn str)

