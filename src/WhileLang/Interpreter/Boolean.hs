module WhileLang.Interpreter.Boolean where

import WhileLang.Syntax (BoolExpr(..))
import WhileLang.Interpreter.Arithmetic (evalArith)

import WhileLang.Interpreter.Runtime (InterpreterMonad)

evalBool :: BoolExpr -> InterpreterMonad Bool
evalBool (BoolLiteral b) = return b
evalBool (Not b) = not <$> evalBool b
evalBool (And b1 b2) = (&&) <$> evalBool b1 <*> evalBool b2
evalBool (Or b1 b2) = (||) <$> evalBool b1 <*> evalBool b2
evalBool (Eq a1 a2) = (==) <$> evalArith a1 <*> evalArith a2
evalBool (Lt a1 a2) = (<) <$> evalArith a1 <*> evalArith a2
evalBool (Gt a1 a2) = (>) <$> evalArith a1 <*> evalArith a2
evalBool (Leq a1 a2) = (<=) <$> evalArith a1 <*> evalArith a2
evalBool (Geq a1 a2) = (>=) <$> evalArith a1 <*> evalArith a2

