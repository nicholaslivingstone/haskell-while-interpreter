module Interpreter.ArithmeticInterpSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Interpreter.ArithmeticInterp
import Interpreter.WhileInterpreter
import WhileLanguage (ArithExpr(..))
import Control.Monad.State
import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "Arithmetic Interpreter" $ do
    it "(Sanity Check) 2 + 2 == 4" $ do
        let res = evalState (interpArith (Add (Num 2) (Num 2))) emptyVarMap
        res `shouldBe` Right 4
    -- simple arithmetic operations
    it "Addition" $ property $
        \x y -> evalState (interpArith (Add (Num x) (Num y))) emptyVarMap == Right (x + y)
    it "Subtraction" $ property $
        \x y -> evalState (interpArith (Sub (Num x) (Num y))) emptyVarMap == Right (x - y)
    it "Multiplication" $ property $
        \x y -> evalState (interpArith (Mul (Num x) (Num y))) emptyVarMap == Right (x * y)
    it "Division" $ do
        pendingWith "Integer division not implemented"
    it "Negation" $ property $
        \x -> evalState (interpArith (Neg (Num x))) emptyVarMap == Right (-x)
    -- variable lookup
    it "Variable Lookup (x + y) * (- z)" $ property $
        \x y z ->
          let varmap = Map.fromList [("x", x), ("y", y), ("z", z)]
              expr = Mul (Add (Var "x") (Var "y")) (Neg (Var "z"))
          in evalState (interpArith expr) varmap == Right ((x + y) * (-z))
    it "Invalid Variable Lookup" $ do
        let res = evalState (interpArith (Var "x")) emptyVarMap
        res `shouldBe` Left (VarNotFound "x")
    -- TODO: Add more tests
