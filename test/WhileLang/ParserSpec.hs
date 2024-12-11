{-# LANGUAGE OverloadedStrings #-}
module WhileLang.ParserSpec (spec) where

import Data.Text
import Text.Megaparsec
import Test.Hspec
import Test.QuickCheck
import WhileLang.Parser
import WhileLang.Syntax
import Control.Monad.State

import qualified Data.Map as Map
import Test.QuickCheck.State (State(expected))

expressNum :: Int -> ArithExpr
expressNum n = if n < 0 then Neg (Num (abs n)) else Num n

runGeneralParseTest :: (Show a, Eq a) => Parser a -> a -> String -> Expectation
runGeneralParseTest parser expected input = do
  let result = runParser parser "" (pack input)
  result `shouldBe` Right expected

runArithParseTest = runGeneralParseTest pArithExpr
runBoolParseTest = runGeneralParseTest pBoolExpr

boolToWhileText :: Bool -> String
boolToWhileText True = "true"
boolToWhileText False = "false"

spec :: Spec
spec = do
  describe "Arithmetic Parsing" $ do
    it "Variable" $ do
      let varExpr = Var "x"
          varText = "x"
          res = runParser pArithExpr "" varText
      res `shouldBe` Right varExpr
    it "Parses random integer" $ property $ \n -> ioProperty $ runArithParseTest
      (expressNum n)
      (show n)
    it "Addition" $ property $ \x y -> ioProperty $ runArithParseTest
     (Add (expressNum x) (expressNum y))
      (show x ++ "+" ++ show y)
    it "Subtraction" $ property $ \x y -> ioProperty $ runArithParseTest
      (Sub (expressNum x) (expressNum y))
      (show x ++ " - " ++ show y)
    it "Multiplication" $ property $ \x y -> ioProperty $ runArithParseTest
      (Mul (expressNum x) (expressNum y))
      (show x ++ "*" ++ show y)
    it "Division" $ property $ \x y -> ioProperty $ runArithParseTest
      (Div (expressNum x) (expressNum y))
      (show x ++ "/" ++ show y)
    it "Negation" $ property $ \x -> ioProperty $ runArithParseTest
      (Neg (expressNum x))
      ("-(" ++ show x ++ ")")
    it "Multi-Operator" $ property $ \x y z -> ioProperty $ runArithParseTest
      (Add (expressNum x) (Mul (expressNum y) (expressNum z)))
      (show x ++ "+" ++ show y ++ "*" ++ show z)
    it "Parentheses" $ property $ \x y z -> ioProperty $ runArithParseTest
      (Add (Mul (expressNum x) (expressNum y)) (expressNum z))
      (show x ++ "*" ++ show y ++ "+" ++ show z)
  describe "Boolean Parsing" $ do
    it "Boolean Literals" $ property $ \b -> ioProperty $ runBoolParseTest
      (BoolLiteral b)
      (boolToWhileText b)

    it "Equality" $ property $ \x y -> ioProperty $ runBoolParseTest
      (Eq (expressNum x) (expressNum y))
      (show x ++ " == " ++ show y)

    it "Less Than" $ property $ \x y -> ioProperty $ runBoolParseTest
      (Lt (expressNum x) (expressNum y))
      (show x ++ " < " ++ show y)

    it "Greater Than" $ property $ \x y -> ioProperty $ runBoolParseTest
      (Gt (expressNum x) (expressNum y))
      (show x ++ " > " ++ show y)

    it "Less Than or Equal" $ property $ \x y -> ioProperty $ runBoolParseTest
      (Leq (expressNum x) (expressNum y))
      (show x ++ " <= " ++ show y)

    it "Greater Than or Equal" $ property $ \x y -> ioProperty $ runBoolParseTest
      (Geq (expressNum x) (expressNum y))
      (show x ++ " >= " ++ show y)

    it "Negation" $ property $ \b -> ioProperty $ runBoolParseTest
      (Not (BoolLiteral b))
      ("not " ++ boolToWhileText b)

    it "Logical AND" $ property $ \b1 b2 -> ioProperty $ runBoolParseTest
      (And (BoolLiteral b1) (BoolLiteral b2))
      (boolToWhileText b1 ++ " and " ++ boolToWhileText b2)

    it "Logical OR" $ property $ \b1 b2 -> ioProperty $ runBoolParseTest
      (Or (BoolLiteral b1) (BoolLiteral b2))
      (boolToWhileText b1 ++ " or " ++ boolToWhileText b2)

    it "Combined Logical Expressions" $ property $ \b1 b2 -> ioProperty $ runBoolParseTest
      (Or (And (BoolLiteral b1) (BoolLiteral b2)) (Not (BoolLiteral b1)))
      ("[" ++ boolToWhileText b1 ++ " and " ++ boolToWhileText b2 ++ "] or not " ++ boolToWhileText b1)
