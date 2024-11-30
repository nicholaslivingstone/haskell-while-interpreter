module Expr where

data ArithExpr
  = Num Int
  | Var String
  | Add ArithExpr ArithExpr
  | Sub ArithExpr ArithExpr
  | Mul ArithExpr ArithExpr
  | Neg ArithExpr 
  deriving (Show, Eq)

data BoolExpr
  = BoolLiteral Bool
  | Not BoolExpr
  | And BoolExpr BoolExpr
  | Or BoolExpr BoolExpr
  | Eq ArithExpr ArithExpr
  | Lt ArithExpr ArithExpr
  | Gt ArithExpr ArithExpr
  | Leq ArithExpr ArithExpr
  | Geq ArithExpr ArithExpr
  deriving (Show, Eq)
