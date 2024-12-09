module WhileLang.InterpreterSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import WhileLang.Interpreter
import WhileLang.Syntax
import Control.Monad.State
import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "Arithmetic Evalutation" $ do
    it "(Sanity Check) 2 + 2 == 4" $ do
        res <- evalArithWithState (Add (Num 2) (Num 2)) emptyVarMap
        res `shouldBe` Right 4
    -- Simple arithmetic operations
    it "Addition" $ property $
        \x y -> ioProperty $ do
            res <- evalArithWithState (Add (Num x) (Num y)) emptyVarMap
            return $ res == Right (x + y)
    it "Subtraction" $ property $
        \x y -> ioProperty $ do
            res <- evalArithWithState (Sub (Num x) (Num y)) emptyVarMap
            return $ res == Right (x - y)
    it "Multiplication" $ property $
        \x y -> ioProperty $ do
            res <- evalArithWithState (Mul (Num x) (Num y)) emptyVarMap
            return $ res == Right (x * y)
    it "Division" $ property $
        \x y -> ioProperty $ do
            let expected = if y == 0
                then Left DivByZero
                else Right (x `div` y)
            res <- evalArithWithState (Div (Num x) (Num y)) emptyVarMap
            return $ res == expected
    it "Negation" $ property $
        \x -> ioProperty $ do
            res <- evalArithWithState (Neg (Num x)) emptyVarMap
            return $ res == Right (-x)
    it "Variable Lookup (x + y) * (- z)" $ property $
       \x y z -> ioProperty $ do
          let varmap = Map.fromList [("x", x), ("y", y), ("z", z)]
              expr = Mul (Add (Var "x") (Var "y")) (Neg (Var "z"))
          res <- evalArithWithState expr varmap
          return $ res == Right ((x + y) * (-z))
    it "Invalid Variable Lookup" $ do
        res <- evalArithWithState (Var "x") emptyVarMap
        res `shouldBe` Left (VarNotFound "x")

