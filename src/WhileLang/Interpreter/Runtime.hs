module WhileLang.Interpreter.Runtime where

import WhileLang.Syntax 
import WhileLang.Interpreter.Boolean
import WhileLang.Interpreter.Arithmetic (interpArith)

import qualified Data.Map as Map


-- Program State
data ProgState = 
  ProgState { 
    varMap :: VarMap,
    err :: Maybe String,
    done :: Bool
  }


-- statement interpreter
interpcommand :: command -> varmap -> varmap
interpstm (assign var val) varmap = map.insert var (interparith val varmap) varmap
interpstm (if bx stm1 stm2) varmap
  | interpbool bx = interpcommand stm1 varmap
  | otherwise = interpcommand stm2 varmap
interpcommand (while bx stm) varmap
  | interpbool bx = interpcommand (while bx stm) (interpcommand stm varmap)
  | otherwise = varmap
interpcommand (seq []) varmap = varmap
interpcommand (seq (stm:stms)) varmap = interpcommand (seq stms) (interpcommand stm varmap)

interp :: Command -> VarMap
interp stm = interpCommand stm emptyVarMap
