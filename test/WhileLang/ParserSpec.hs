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
import GHC.IO.Handle (NewlineMode(inputNL))

expressNum :: Int -> ArithExpr
expressNum n = if n < 0 then Neg (Num (abs n)) else Num n 

runArithParseTest :: ArithExpr -> String -> Expectation
runArithParseTest expected input = do
  let result = runParser pArithExpr "" (pack input)
  result `shouldBe` Right expected

spec :: Spec
spec = do
  describe "Parser" $ do
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
