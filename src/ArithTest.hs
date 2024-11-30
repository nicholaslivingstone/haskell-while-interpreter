module ArithTest where

import Control.Monad.State
import qualified Data.Map as Map
import Interpreter.ArithmeticInterp
import WhileProgram

-- Example: x + y
exampleExpr :: ArithExpr
exampleExpr = Add (Var "x") (Var "y")

-- Initial state with variable values
initialState :: VarMap
initialState = Map.fromList [("x", 10), ("y", 5)]

-- Run the program
main :: IO ()
main = do
  let result = evalState (interpArith exampleExpr) initialState
  case result of
    Right value -> putStrLn $ "Result: " ++ show value
    Left error  -> putStrLn $ "Error: " ++ error
