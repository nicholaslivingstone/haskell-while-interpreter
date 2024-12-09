module Command where

import WhileLang.Syntax (Command(..))
import WhileLang.Interpreter.Boolean (evalBool)
import WhileLang.Interpreter.Arithmetic (evalArith)
import WhileLang.Interpreter.Runtime (InterpreterMonad)
import Control.Monad.State (liftIO, modify)
import qualified Data.Map as Map

-- Command Interpretation
interpCommand :: Command -> InterpreterMonad ()
interpCommand (Assign var expr) = do
  val <- evalArith expr
  modify (Map.insert var val)

interpCommand Skip = return ()

interpCommand (If cond thenCmd elseCmd) = do
  p <- evalBool cond
  let targetCmd = if p then thenCmd else elseCmd
  interpCommand targetCmd

interpCommand (While cond cmd) = do
-- TODO: Implement While loop
  return ()

interpCommand (Seq cmds) = mapM_ interpCommand cmds

interpCommand (Print str) = liftIO (putStrLn str)
