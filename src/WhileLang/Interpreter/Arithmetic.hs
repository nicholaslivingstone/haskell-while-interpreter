module WhileLang.Interpreter.Arithmetic where

import WhileLang.Syntax (ArithExpr(..))
import WhileLang.Interpreter.Runtime (InterpreterMonad)
import WhileLang.Interpreter.Core (WhileError(..))
import Control.Monad.State  (MonadState(..), get)
import Control.Monad.Except (throwError)
import qualified Data.Map as Map

evalArith :: ArithExpr -> InterpreterMonad Int
evalArith (Num n) = return n
evalArith (Var v) = do
  varMap <- get
  case Map.lookup v varMap of
    Just n -> return n
    Nothing -> throwError (VarNotFound v)
evalArith (Add a1 a2) = liftBinOp (+) a1 a2
evalArith (Sub a1 a2) = liftBinOp (-) a1 a2
evalArith (Mul a1 a2) = liftBinOp (*) a1 a2
evalArith (Div a1 a2) = do
  n1 <- evalArith a1
  n2 <- evalArith a2
  if n2 == 0
    then throwError DivByZero
    else return (n1 `div` n2)
evalArith (Neg a) = do
  n <- evalArith a
  return (-n)

liftBinOp :: (Int -> Int -> Int) -> ArithExpr -> ArithExpr -> InterpreterMonad Int
liftBinOp op a1 a2 = do
  n1 <- evalArith a1
  n2 <- evalArith a2
  return (n1 `op` n2)

