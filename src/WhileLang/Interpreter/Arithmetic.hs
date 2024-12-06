module WhileLang.Interpreter.Arithmetic where

import WhileLang.Syntax
import WhileLang.Interpreter.Core (VarMap, WhileError(..))

import Control.Monad.State
import qualified Data.Map as Map

type ArithResult = Either WhileError Int

-- Arithmetic Expression Interpreter
interpArith :: ArithExpr -> State VarMap ArithResult
interpArith (Num n) = return (Right n)
interpArith (Var v) = do
  varmap <- get
  case Map.lookup v varmap of
    Just n  -> return (Right n)
    Nothing -> return (Left (VarNotFound v))
interpArith (Add ax1 ax2) = interpBinOp (+) ax1 ax2
interpArith (Sub ax1 ax2) = interpBinOp (-) ax1 ax2
interpArith (Mul ax1 ax2) = interpBinOp (*) ax1 ax2
-- TODO: Handle division by zero
-- TODO: Handle non-integer division
interpArith (Div ax1 ax2) = interpBinOp div ax1 ax2
interpArith (Neg ax) = interpUnaryOp negate ax

-- Interpreter Binary Operation
interpBinOp :: (Int -> Int -> Int) -> ArithExpr -> ArithExpr -> State VarMap ArithResult
interpBinOp op ax1 ax2 = do
  r1 <- interpArith ax1
  r2 <- interpArith ax2
  return $ (op <$> r1) <*> r2

-- Interpret Unary Operation
interpUnaryOp :: (Int -> Int) -> ArithExpr -> State VarMap ArithResult
interpUnaryOp op ax = do
  r <- interpArith ax
  return $ fmap op r

