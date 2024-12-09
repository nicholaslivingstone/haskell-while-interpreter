module Command where

import WhileLang.Syntax 
import WhileLang.Interpreter.Boolean
import WhileLang.Interpreter.Arithmetic (interpArith)
import WhileLang.Interpreter.Runtime (InterpreterMonad)

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
-- TODO: Implement While loop
  return ()

interpCommand (Seq cmds) = mapM_ interpCommand cmds

interpCommand (Print str) = liftIO (putStrLn str)
