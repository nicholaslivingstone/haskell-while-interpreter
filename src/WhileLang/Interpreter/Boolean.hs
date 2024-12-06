module WhileLang.Interpreter.Boolean where

import WhileLang.Syntax
import WhileLang.Interpreter.Core (VarMap, WhileError(..))
import WhileLang.Interpreter.Arithmetic (interpArith)

import Control.Monad.State


type BoolResult = Either WhileError Bool

interpBool :: BoolExpr -> State VarMap BoolResult
interpBool (BoolLiteral b) = return (Right b)
interpBool (Not b) = interpUnaryOp not b
interpBool (And b1 b2) = interpBinOp (&&) b1 b2
interpBool (Or b1 b2) = interpBinOp (||) b1 b2
interpBool (Eq a1 a2) = interpBinArithOp (==) a1 a2
interpBool (Lt a1 a2) = interpBinArithOp (<) a1 a2
interpBool (Gt a1 a2) = interpBinArithOp (>) a1 a2
interpBool (Leq a1 a2) = interpBinArithOp (<=) a1 a2
interpBool (Geq a1 a2) = interpBinArithOp (>=) a1 a2

interpBinOp :: (Bool -> Bool -> Bool) -> BoolExpr -> BoolExpr -> State VarMap BoolResult
interpBinOp op b1 b2 = do
  r1 <- interpBool b1
  r2 <- interpBool b2
  return $ (op <$> r1) <*> r2

interpBinArithOp :: (Int -> Int -> Bool) -> ArithExpr -> ArithExpr -> State VarMap BoolResult
interpBinArithOp op a1 a2 = do
  r1 <- interpArith a1
  r2 <- interpArith a2
  return $ (op <$> r1) <*> r2

interpUnaryOp :: (Bool -> Bool) -> BoolExpr -> State VarMap BoolResult
interpUnaryOp op b = do
  r <- interpBool b
  return $ fmap op r

