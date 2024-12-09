module WhileLang.Interpreter where

import WhileLang.Syntax
import Control.Monad (when)
import Control.Monad.State (StateT, runStateT, get, modify, liftIO)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import qualified Data.Map as Map

-- Variables
type VarName = String
type VarMap = Map.Map VarName Int
emptyVarMap :: VarMap
emptyVarMap = Map.empty

-- Errors
data WhileError
  = VarNotFound VarName
  | DivByZero
  | OtherError String
  deriving (Eq)

instance Show WhileError where
  show (VarNotFound v) = "Variable not found: " ++ v
  show DivByZero = "Division by zero"
  show (OtherError s) = s

-- The Magical Monad Stack for the Interpreter
-- (StateT VarMap IO) Allows IO actions while managing variable states
-- (ExceptT WhileError) Allows error propogation using custom error type
type InterpreterMonad = ExceptT WhileError (StateT VarMap IO)

-- Interpreter Entry Point
runInterpreter :: Command -> IO (Either WhileError VarMap)
runInterpreter command = do
  (result, finalState) <- runStateT (runExceptT (interpCommand command)) emptyVarMap
  case result of
    Left err -> return (Left err)
    Right () -> return (Right finalState)

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
interpCommand (While cond cmd) = loop
  where
    loop = do
      condVal <- evalBool cond
      when condVal $ do
        interpCommand cmd
        loop
interpCommand (Seq cmds) = mapM_ interpCommand cmds
interpCommand (Print str) = liftIO (putStrLn str)

-- Boolean Expression Evaluation
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

-- Arithmetic Expression Evaluation
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

-- For Testing
evalArithWithState :: ArithExpr -> VarMap -> IO (Either WhileError Int)
evalArithWithState expr varMap =
  runStateT (runExceptT (evalArith expr)) varMap >>= \(result, _) -> return result
