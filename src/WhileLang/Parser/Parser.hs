{-
 - Disclaimer: Quite a bit of this code comes from the megaparsec tutorial found here: 
 - https://markkarpov.com/tutorial/megaparsec.html
  -}

{-# LANGUAGE OverloadedStrings #-}

module WhileLang.Parser.Parser where

import WhileLang.Syntax

import Data.Text (Text)
import Data.Void
import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- Space Consumer
sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "--")
  (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- Operator Boilerplate
binary :: Text -> (a -> a -> a) -> Operator Parser a
binary  name f = InfixL  (f <$ symbol name)

prefix, postfix :: Text -> (a -> a) -> Operator Parser a 
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

-- Arithmetic Expressions
pVariable :: Parser ArithExpr
pVariable = Var <$> lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pInteger :: Parser ArithExpr
pInteger =  Num <$> lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pArithTerm :: Parser ArithExpr
pArithTerm = choice
  [ pVariable
  , pInteger
  , parens pArithExpr
  ]

arithOpTable :: [[Operator Parser ArithExpr]]
arithOpTable = 
  [ [ prefix "-" Neg
    , prefix "+" id
    ]
  , [ binary "*" Mul
    , binary "/" Div
    ]
  , [ binary "+" Add
    , binary "-" Sub
    ]
  ]

pArithExpr :: Parser ArithExpr
pArithExpr = makeExprParser pArithTerm arithOpTable

