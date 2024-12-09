module WhileLang.InterpreterSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import WhileLang.Interpreter
import WhileLang.Syntax
import Control.Monad.State
import qualified Data.Map as Map

arithmeticTest :: String -> (ArithExpr -> ArithExpr -> ArithExpr) -> (Int -> Int -> Int) -> Spec
arithmeticTest name exprConstructor op =
    it name $ property $ \x y -> ioProperty $ do
        res <- evalArithWithState (exprConstructor (Num x) (Num y)) emptyVarMap
        return $ res == Right (x `op` y)

booleanTest name exprConstructor op =
    it name $ property $ \b1 b2 -> ioProperty $ do
        res <- evalBoolWithState (exprConstructor (BoolLiteral b1) (BoolLiteral b2)) emptyVarMap
        return $ res == Right (b1 `op` b2)

comparisonTest :: String -> (ArithExpr -> ArithExpr -> BoolExpr) -> (Int -> Int -> Bool) -> Spec
comparisonTest name exprConstructor op =
    it name $ property $ \x y -> ioProperty $ do
        res <- evalBoolWithState (exprConstructor (Num x) (Num y)) emptyVarMap
        return $ res == Right (x `op` y)


spec :: Spec
spec = do
  describe "Arithmetic Evaluation" $ do
    it "(Sanity Check) 2 + 2 == 4" $ do
        res <- evalArithWithState (Add (Num 2) (Num 2)) emptyVarMap
        res `shouldBe` Right 4

    -- Arithmetic operations
    arithmeticTest "Addition" Add (+)
    arithmeticTest "Subtraction" Sub (-)
    arithmeticTest "Multiplication" Mul (*)
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

  describe "Boolean Evaluation" $ do
    it "(Sanity Check) True AND False == False" $ do
        res <- evalBoolWithState (And (BoolLiteral True) (BoolLiteral False)) emptyVarMap
        res `shouldBe` Right False

    -- Boolean operations
    booleanTest "AND operation" And (&&)
    booleanTest "OR operation" Or (||)

    -- Comparisons
    comparisonTest "Equality" Eq (==)
    comparisonTest "Less Than" Lt (<)
    comparisonTest "Greater Than" Gt (>)
    comparisonTest "Less Than or Equal" Leq (<=)
    comparisonTest "Greater Than or Equal" Geq (>=)

    it "A Complex Boolean Expression (x < y OR NOT(z > 0))" $ property $
        \x y z -> ioProperty $ do
            let varmap = Map.fromList [("x", x), ("y", y), ("z", z)]
                expr = Or (Lt (Var "x") (Var "y")) (Not (Gt (Var "z") (Num 0)))
            res <- evalBoolWithState expr varmap
            return $ res == Right ((x < y) || not (z > 0))

