module WhileProgram where

import qualified Data.Map as Map

-- Program State
data ProgState = 
  ProgState { 
    varMap :: VarMap,
    err :: Maybe String,
    done :: Bool
  }

-- Variables
type VarName = String

type VarMap = Map.Map VarName Int

emptyVarMap :: VarMap
emptyVarMap = Map.empty

-- Commands
data Command
  = Assign String ArithExpr
  | Skip
  | If BoolExpr Command Command
  | While BoolExpr Command
  | Seq [Command]
  | Print String

-- Arithmetic Expressions
data ArithExpr
  = Num Int
  | Var String
  | Add ArithExpr ArithExpr
  | Sub ArithExpr ArithExpr
  | Mul ArithExpr ArithExpr
  | Div ArithExpr ArithExpr
  | Neg ArithExpr 
  deriving (Show, Eq)

-- Boolean Expressions
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



